(ns active.lawrence.direct-lr
  (:require [active.lawrence.grammar :refer :all]
            [active.lawrence.lr :refer :all]
            [active.lawrence.runtime :refer :all]
            [active.clojure.condition :as c]))

;; FIXME: attribution
;; FIXME: error recovery, error

(declare ds-parse-bar)

(defn ds-parse
  [grammar k compute-closure state input]
  (let [closure (compute-closure state grammar k)
        reduce (fn []
                 (if-let [item (find-lookahead-item (accept closure) k input)]
                   (let [rhs-length (count (item-rhs item))
                         lhs (item-lhs item)]
                     (if (zero? rhs-length)
                       (ds-parse-bar grammar k compute-closure
                                     closure lhs input)
                       [lhs rhs-length input]))
                   (c/error `ds-parse "parse error")))]
    (if (empty? input)
      (reduce)
      (let [symbol (pair-token (first input))]
        (if (contains? (next-terminals closure grammar) symbol)
          (ds-parse-bar grammar k compute-closure closure
                        symbol (rest input))
          (reduce))))))

(defn ds-parse-bar
  [grammar k compute-closure closure symbol input]
  (let [the-next-nonterminals (next-nonterminals closure grammar)]
    (let [[lhs dot input] (ds-parse grammar k compute-closure
                                    (goto closure symbol) input)]
      (cond
       (empty? the-next-nonterminals) [lhs (- dot 1) input]

       (> dot 1) [lhs (- dot 1) input]
       
       (and (initial? closure grammar)
            (= (grammar-start grammar) lhs))
       (if (empty? input)
         :accept
         (c/error `ds-parse-bar "parse error"))
       
       :else (recur grammar k compute-closure
                    closure
                    lhs
                    input)))))

(defn parse
  [grammar k method input]
  (let [start-production (grammar-start-production grammar)]
    (ds-parse grammar
	      k
	      (if (= method :lr)
		  (fn [state grammar k]
		    (compute-lr-closure state grammar k))
		  (fn [state grammar k]
		    (compute-slr-closure state grammar k)))
	      #{(make-item start-production 0 '())}
	      input)))
