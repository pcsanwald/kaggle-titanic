(ns titanic.core
(:use [clojure-csv.core]))

(defn group-row 
    "Given a header field, and a row of data, create a map of that data keyed by the header.
    Very similar to zipmap, only my version works with lists as values."
    [header data & {:keys [result] :or {result {}}}]
    (let [hs (seq header) vs (seq data)]
    (cond
    (empty? vs) result
    :else 
        (group-row 
            (rest hs) (rest vs)
            :result (assoc result (first hs) (first vs) ) 
            )))) 

(defn slice-data 
    "Given a bunch of rows of data and a header, return a map keyed by each field in the
    header, where the value for each key is a collection of data across all the rows."
    [header data & {:keys [result] :or {result {}}}]
    (let [row (map list (first data))]
    (cond
    (empty? data) result
    :else
        (slice-data header (rest data) :result (merge-with concat (group-row header row) result))
        )))

(defn gen-possible-values
    "Produce a map that contains all possible values in the data.
    Written to take the output of slice-data, and produce all possible
    values."
    [header datamap & {:keys [result] :or {result {}}}]
    (let [category (first header) remaining (rest header)]
    (cond
    (empty? header) result
    :else 
        (gen-possible-values remaining datamap :result (assoc result category (set (category datamap))))
        )))

(def parsed (parse-csv (slurp "train.csv")))
(def header (map keyword (first parsed)))
(def data (rest parsed))
(def train-by-column (slice-data header data))

; show all the possible values for the embarked column
(:embarked (gen-possible-values (keys train-by-column) train-by-column))
; count how many passengers embarked at a certain location
(count (filter #{"S"} (:embarked train-by-column)))

; for dealing with multiple elements, we can create a pivot table
(def survived-gender 
    (map #(list %1 %2) (:survived train-by-column) (:sex train-by-column)))

(defn total-up [coll predicate]
(count (filter #(= % predicate) coll)))

; this is just to avoid duplicate code for doing the various sums
(def total-sg (partial total-up survived-gender))

; usage:
;(total-sg ["1" "female"])
;(total-sg ["0" "female"])


; ok, time to update the test file with our predictions.
(def test-file (parse-csv (slurp "test.csv")))
(def test-header (first test-file))
(def test-body (rest test-file))

; define the algorithm we're using to make our predictions
(defn update-row [row]
(if (= (row 2) "female") 
    (cons "1" row)
    (cons "0" row)
    ))

; apply the algorithm to the data
(def result-body (map update-row test-body))
(def result-header (cons "Survived" test-header))
(def result (cons result-header result-body))
(def csv (write-csv result))
; (spit "../prediction.csv" csv)

