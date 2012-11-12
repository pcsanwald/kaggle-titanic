# titanic

A Clojure library that generates the same CSV file as done by excel, in Kaggle's 
titanic example: http://www.kaggle.com/c/titanic-gettingStarted/details/getting-started-with-excel

## Usage

lein repl
(use 'titanic.core)
(spit "prediction.csv" csv)

## License

Copyright © 2012 Paul Sanwald

Distributed under the Eclipse Public License, the same as Clojure.
