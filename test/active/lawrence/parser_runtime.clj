(ns active.lawrence.parser-runtime
  (require [active.clojure.condition :as c]))

(defn parse-error
  [message input]
  (c/error `parse-error message (first input)))
  
  
