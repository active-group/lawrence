(ns active.lawrence.direct-lr
  (:require [active.lawrence.grammar :refer :all]
            [active.lawrence.lr :refer :all]
            [active.lawrence.runtime :refer :all]
            [active.clojure.condition :as c]))

;; only correct number of attributes
;; FIXME: error recovery, error

(declare ds-parse-bar)

(defn apply-attribution
  [attr vals]
  ;; FIXME: need eval or something
  (apply attr vals))

(defn ds-parse
  [grammar k compute-closure state attribute-values input]
  (let [closure (compute-closure state grammar k)
        reduce (fn []
                 (if-let [item (find-lookahead-item (accept closure) k input)]
                   (let [rhs-length (count (item-rhs item))
                         lhs (item-lhs item)
                         attribution (production-attribution (item-production item))
                         attribute-value (apply-attribution attribution
                                                            (reverse (take rhs-length attribute-values)))]
                     
                     (if (zero? rhs-length) ;; FIXME: zap this case?
                       (ds-parse-bar grammar k compute-closure
                                     closure lhs (list attribute-value) input)
                       [lhs rhs-length attribute-value input]))
                   (c/error `ds-parse "parse error")))]
    (if (empty? input)
      (reduce)
      (let [pair (first input)
            symbol (pair-token pair)]
        (if (contains? (next-terminals closure grammar) symbol)
          (ds-parse-bar grammar k compute-closure closure
                        symbol
                        (pair-attribute-value pair) 
                        attribute-values
                        (rest input))
          (reduce))))))

(defn ds-parse-bar
  [grammar k compute-closure closure symbol attribute-value attribute-values input]
  (let [the-next-nonterminals (next-nonterminals closure grammar)]
    (let [[lhs dot attribute-value input] (ds-parse grammar k compute-closure
                                                    (goto closure symbol) (cons attribute-value attribute-values) input)]
      (cond
       (empty? the-next-nonterminals) [lhs (- dot 1) attribute-value input]

       (> dot 1) [lhs (- dot 1) attribute-value input]
       
       (and (initial? closure grammar)
            (= (grammar-start grammar) lhs))
       (if (empty? input)
         attribute-value
         (c/error `ds-parse-bar "parse error"))
       
       :else (recur grammar k compute-closure
                    closure
                    lhs
                    attribute-value
                    attribute-values
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
              '()
	      input)))
