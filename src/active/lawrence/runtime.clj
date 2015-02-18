(ns active.lawrence.runtime)

(deftype Pair [token attribute-value])

(defn make-pair
  [t av]
  (Pair. t av))

(defn pair-token
  [^Pair p]
  (.token p))

(defn pair-attribute-value
  [^Pair p]
  (.attribute-value p))



