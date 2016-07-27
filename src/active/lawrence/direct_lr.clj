(ns active.lawrence.direct-lr
  (:require [active.lawrence.grammar :refer :all]
            [active.lawrence.lr :refer :all]
            [active.lawrence.runtime :refer :all]
            [active.clojure.condition :as c])
  (:import [active.lawrence.runtime RetVal]))

(defn find-lookahead-item
  [item-set k input]
  (loop [item-set (sort item<? item-set)]
    (if (empty? item-set)
      false
      (let [item (first item-set)]
        (if (lookahead-matches? k (item-lookahead item) input)
          item
          (recur (rest item-set)))))))

(declare ds-parse-bar ds-handle-error)

(defn ds-parse
  ^RetVal [grammar k compute-closure state attribute-values error-status input]
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
                       (ds-parse-bar grammar k compute-closure closure lhs attribute-value error-status attribute-values input)
                       (->RetVal lhs rhs-length attribute-value error-status input)))
                   (if (handles-error? closure grammar)
                     (ds-handle-error grammar k compute-closure error-status closure attribute-values input)
                     (->RetVal -1 0 nil error-status input))))]
    (if (empty? input)
      (reduce)
      (let [pair (first input)
            symbol (pair-token pair)]
        (if (contains? (next-terminals closure grammar) symbol)
          (ds-parse-bar grammar k compute-closure closure
                        symbol
                        (pair-attribute-value pair) 
                        attribute-values
                        (if (zero? error-status)
                          error-status
                          (- error-status 1))
                        (rest input))
          (reduce))))))

(defn ds-parse-bar
  ^RetVal [grammar k compute-closure closure symbol attribute-value attribute-values error-status input]
  {:pre [(= (active closure) (count attribute-values))]}
  (let [next-state (goto closure symbol)
        retval (ds-parse grammar k compute-closure
                         next-state
                         (take (active next-state)
                               (cons attribute-value attribute-values))
                         error-status
                         input)]

    (cond (neg? (.-lhs retval)) ; error
          (if (handles-error? closure grammar)
            (ds-handle-error grammar k compute-closure error-status closure attribute-values input)
            retval)
      
          (empty? (next-nonterminals closure grammar))
          (dec-dot retval)

          (> (.dot retval) 1) 
          (dec-dot retval)
       
          (and (initial? closure grammar)
               (= (grammar-start grammar) (.-lhs retval)))
          (if (empty? (.-input retval))
            retval
            (c/error `ds-parse-bar "excess input after EOF"))
       
          :else (recur grammar k compute-closure
                       closure
                       (.lhs retval) ;; guaranteed to be a member of next nonterminals
                       (.-attribute-value retval)
                       attribute-values
                       error-status
                       (.-input retval)))))

(defn ds-handle-error
  [grammar k compute-closure error-status closure attribute-values start-input]
  {:pre [(handles-error? closure grammar)]}
  (let [next-state (goto closure (grammar-error grammar))
        keep (- (active next-state) 1)
        next-closure (compute-closure next-state grammar k)
        next-accept-items (accept next-closure)]
    (loop [input start-input]

      (let [recover (fn []
                      (ds-parse grammar k compute-closure
                                next-state
                                (take (active next-state)
                                      (cons nil attribute-values))
                                3
                                input))]
                      
        (if (empty? input)
          (if (find-eoi-lookahead-item next-accept-items)
            (recover)
            (c/error `ds-handle-error "premature end of input"))
          
          (let [pair (first input)
                symbol (pair-token pair)]
            (if (or (contains? (next-terminals next-closure grammar) symbol)
                    (find-lookahead-item next-accept-items k input))
              (recover)
              (recur (rest input)))))))))

(defn parse
  [grammar k method input]
  (let [retval
        (ds-parse grammar
             k
             (case method
               :lr compute-lr-closure
               :slr compute-slr-closure)
             #{(make-item (grammar-start-production grammar) 0 '())}
             '()
             3
             input)]
    (if (neg? (.-lhs retval))
      (c/error `parse "parse error")
      (.-attribute-value retval))))

