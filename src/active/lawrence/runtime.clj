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

(definterface IRetVal
  (dot [])
  (set_dot [x]))

(deftype RetVal
    [lhs ^:unsynchronized-mutable dot attribute-value input]
  IRetVal
  (dot [_] dot)
  (set_dot [_ x] (set! dot x)))

(defn dec-dot
  [^RetVal rv]
  (.set_dot rv (dec (.dot rv)))
  rv)

(defn apply-attribution
  [attr vals]
  (apply (eval attr) vals))



