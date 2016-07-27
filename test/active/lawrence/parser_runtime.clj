(ns active.lawrence.parser-runtime
  (require [active.clojure.condition :as c]))

(defn parse-error
  [message expected input]
  (c/error `parse-error message expected (first input)))
  
  
