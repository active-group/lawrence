(ns active.lawrence.direct-lr
  (:require [active.lawrence.grammar :refer :all]
            [active.lawrence.lr :refer :all]
            [active.lawrence.runtime :refer :all]
            [active.clojure.condition :as c]))

;; FIXME: error recovery, error

(declare ds-parse-bar)

(defn ds-parse
  [grammar k compute-closure state attribute-values input]
  {:pre [(= (active state) (count attribute-values))]}
  (let [closure (compute-closure state grammar k)
        reduce (fn []
                 (if-let [item (find-lookahead-item (accept closure) k input)]
                   (let [rhs-length (count (item-rhs item))
                         lhs (item-lhs item)
                         attribution (production-attribution (item-production item))
                         attribute-value (apply-attribution attribution
                                                            (reverse (take rhs-length attribute-values)))]
                     
                     (->RetVal lhs rhs-length attribute-value input))
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
  (let [next-state (goto closure symbol)
        retval (ds-parse grammar k compute-closure
                         next-state
                         (cons attribute-value
                               (take (- (active next-state) 1) attribute-values))
                         input)]
  
    (if (empty? (next-nonterminals closure grammar))
      (dec-dot retval)
      (cond
       (> (.dot retval) 1) 
       (dec-dot retval)
       
       (and (initial? closure grammar)
            (= (grammar-start grammar) (.-lhs retval)))
       (if (empty? (.-input retval))
         (.-attribute-value retval)
         (c/error `ds-parse-bar "parse error"))
       
       :else (recur grammar k compute-closure
                    closure
                    (.lhs retval) ;; guaranteed to be a member of next nonterminals
                    (.-attribute-value retval)
                    attribute-values
                    (.-input retval))))))

(defn parse
  [grammar k method input]
  (let [start-production (grammar-start-production grammar)]
    (ds-parse grammar
	      k
              (case method
                :lr compute-lr-closure
                :slr compute-slr-closure)
	      #{(make-item start-production 0 '())}
              '()
	      input)))
