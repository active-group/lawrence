(ns active.lawrence.direct-lr
  (:require [active.lawrence.grammar :refer :all]
            [active.lawrence.lr :refer :all]
            [active.lawrence.runtime :refer :all]
            [active.clojure.condition :as c])
  (:import [active.lawrence.runtime RetVal]))

;; FIXME: error recovery, error

(defn find-lookahead-item
  [item-set k input]
  (loop [item-set (seq item-set)]
    (if (empty? item-set)
      false
      (let [item (first item-set)]
        (if (lookahead-matches? k (item-lookahead item) input)
          item
          (recur (rest item-set)))))))

(declare ds-parse-bar)

(defn ds-parse
  ^RetVal [grammar k compute-closure state attribute-values input]
  {:pre [(= (active state) (count attribute-values))]}
  (let [closure (compute-closure state grammar k)
        reduce (fn []
                 (if-let [item (find-lookahead-item (accept closure) k input)]
                   (let [rhs-length (count (item-rhs item))
                         lhs (item-lhs item)
                         attribution (production-attribution (item-production item))
                         attribute-value (apply-attribution attribution
                                                            (reverse (take rhs-length attribute-values)))]
                     (if (zero? rhs-length)
                       (ds-parse-bar grammar k compute-closure closure lhs attribute-value attribute-values input)
                       (->RetVal lhs rhs-length attribute-value input)))
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
  ^RetVal [grammar k compute-closure closure symbol attribute-value attribute-values input]
  {:pre [(= (active closure) (count attribute-values))]}
  (let [next-state (goto closure symbol)
        retval (ds-parse grammar k compute-closure
                         next-state
                         (take (active next-state)
                               (cons attribute-value attribute-values))
                         input)]
    (if (empty? (next-nonterminals closure grammar))
      (dec-dot retval)
      (cond
       (> (.dot retval) 1) 
       (dec-dot retval)
       
       (and (initial? closure grammar)
            (= (grammar-start grammar) (.-lhs retval)))
       (if (empty? (.-input retval))
         retval
         (c/error `ds-parse-bar "parse error"))
       
       :else (recur grammar k compute-closure
                    closure
                    (.lhs retval) ;; guaranteed to be a member of next nonterminals
                    (.-attribute-value retval)
                    attribute-values
                    (.-input retval))))))

(defn parse
  [grammar k method input]
  (.-attribute-value
   (ds-parse grammar
             k
             (case method
               :lr compute-lr-closure
                                   :slr compute-slr-closure)
             #{(make-item (grammar-start-production grammar) 0 '())}
             '()
             input)))
