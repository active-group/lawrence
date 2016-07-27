(ns active.lawrence.generate-test
  (:require [active.lawrence.grammar-test :refer :all]
            [active.lawrence.lr :as lr]))

(defn generate
  [grammar name method]
  (lr/write-ds-parse-ns grammar 1 method
                        (symbol (str "active.lawrence." name "-parser"))
                        '([active.lawrence.parser-runtime :refer :all])
                        (str "test/active/lawrence/" name "_parser.clj")))
