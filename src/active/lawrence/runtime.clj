(ns active.lawrence.runtime)

(deftype Pair [^long token attribute-value])

(defn make-pair
  [t av]
  (Pair. t av))

(defn pair-token
  ^long [^Pair p]
  (.token p))

(defn pair-attribute-value
  [^Pair p]
  (.attribute-value p))

(definterface IRetVal
  (dot [])
  (set_dot [^int x]))

(deftype RetVal
    [^int lhs ^int ^:unsynchronized-mutable dot attribute-value input]
  IRetVal
  (dot [_] dot)
  (set_dot [_ ^int x] (set! dot x)))

(defn dec-dot
  [^RetVal rv]
  (.set_dot rv (dec (.dot rv)))
  rv)

(defn apply-attribution
  [attr vals]
  (apply (eval attr) vals))

; Lookahead

(defn lookahead-matches?
  [k lookahead input]
  (loop [k k
         lookahead (seq lookahead) ;; FIXME: seq necessary
         input (seq input)]
    (cond
     (zero? k) true
     (empty? lookahead) (empty? input)
     (empty? input) false
     (= (first lookahead) (pair-token (first input)))
     (recur (- k 1) (rest lookahead) (rest input))
     :else false)))



