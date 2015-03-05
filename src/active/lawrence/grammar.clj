(ns active.lawrence.grammar
  (:require [clojure.set :as set]
            [active.clojure.record :refer :all]
            [active.lawrence.scc-union :as scc-union]))

; Start-separated grammars

; Guarantees:

; - the grammar symbols are numbers.
; - nonterminals and terminals are consecutive, respectively
; - the nonterminals come before the terminals
; - the start production is first

(define-record-type Grammar
  (really-make-grammar name
		       nonterminals
		       terminals
		       number-of-terminals
		       number-of-symbols
		       error start
		       productions-by-lhs
		       symbol->name-procedure
		       name->symbol-procedure
		       terminal-attribution
		       properties)
  grammar?
  [name grammar-name
   nonterminals grammar-nonterminals
   terminals grammar-terminals
   number-of-terminals grammar-number-of-terminals
   number-of-symbols grammar-number-of-symbols
   error grammar-error
   start grammar-start
   productions-by-lhs grammar-productions-by-lhs
   symbol->name-procedure grammar-symbol->name-procedure
   name->symbol-procedure grammar-name->symbol-procedure
   terminal-attribution grammar-terminal-attribution
   properties grammar-properties])

(defn grammar-number-of-nonterminals
  [grammar]
  (- (grammar-number-of-symbols grammar)
     (grammar-number-of-terminals grammar)))

(defn grammar-nonterminal-offset
  [grammar]
  (first (grammar-nonterminals grammar)))

;; at least nonterminals must be sorted
(defn make-grammar
  [name
   nonterminals terminals
   error start
   productions-by-lhs ; vector, indexed by normalized non-terminal
   symbol->name-procedure
   name->symbol-procedure
   terminal-attribution]
  (let [number-of-nonterminals (count nonterminals)
        number-of-terminals (count terminals)
        number-of-symbols (+ number-of-terminals number-of-nonterminals)]
    
    (really-make-grammar name
			 nonterminals terminals
			 number-of-terminals
			 number-of-symbols
			 error start
			 productions-by-lhs
			 symbol->name-procedure
			 name->symbol-procedure
			 terminal-attribution
			 (atom {}))))

(defn grammar-fetch-property
  [grammar name proc]
  (or (get @(grammar-properties grammar) name)
      (let [value (proc grammar)]
        (swap! (grammar-properties grammar)
               assoc name value)
        value)))

(defn grammar-symbol->name
  [symbol grammar]
  ((grammar-symbol->name-procedure grammar) symbol))

(defn grammar-name->symbol
  [symbol grammar]
  ((grammar-name->symbol-procedure grammar) symbol))

(defmacro terminal
  [?grammar ?terminal]
  ((grammar-name->symbol-procedure (eval ?grammar)) ?terminal))

(defn terminal?
  [symbol grammar]
  (< symbol (grammar-number-of-terminals grammar)))

(defn nonterminal?
  [symbol grammar]
  (>= symbol (grammar-number-of-terminals grammar)))

(defn grammar-productions-with-lhs
  [lhs grammar]
  (get (grammar-productions-by-lhs grammar)
       (- lhs (grammar-nonterminal-offset grammar))))

(defn grammar-start-production
  [grammar]
  (first (grammar-productions-with-lhs (grammar-start grammar) grammar)))

(defn grammar-for-each-production
  [proc grammar]
  (let [offset (grammar-nonterminal-offset grammar)
        by-lhs (grammar-productions-by-lhs grammar)
        cnt (grammar-number-of-nonterminals grammar)]
    (loop [i 0]
      (when (< i cnt)
        (doseq [prod (get by-lhs i)]
          (proc prod))
        (recur (+ 1 i))))))

(define-record-type Production
  (make-production readable lhs rhs attribution)
  production?
  [readable production-readable
   lhs production-lhs
   rhs production-rhs
   attribution production-attribution])

; FIXME:We break this abstraction further below in `define-grammar-2'

(defn- grammar-symbol?
  [thing]
  (or (symbol? thing)
      (keyword? thing)))

(defn- production-spec? 
  [spec]
  (and (list? spec)
       (not-empty spec)
       (list? (first spec))
       (every? grammar-symbol? (first spec))
       (not-empty (rest spec))))

(defn- rule?
  [rule]
  (and (list? rule)
       (not-empty rule)
       (symbol? (first rule))
       (every? production-spec? (rest rule))))

(defmacro define-grammar
  [?grammar-name ?terminals ?start-symbol ?rules & [?terminal-attribution]]
  (doseq [p ?rules]
    (when-not (rule? p)
      (throw (IllegalArgumentException. (str "not a grammar rule " *ns* " " (meta &form))))))

  (let [nonterminals (set (map first ?rules))
        symbols (concat [:$error] ?terminals ['$start] nonterminals)
        nonterminal-offset (+ 1 (count ?terminals))
        symbol-table (into {} (map-indexed (fn [i sym] (vector sym i)) symbols))
        productions-by-lhs (vec
                            (concat [`[(make-production '(~'$start (~?start-symbol) ~'identity)
                                                        ~nonterminal-offset
                                                        ~(vector (symbol-table ?start-symbol))
                                                        '~'identity)]]
                                    (map (fn [lhs]
                                           (let [lhs-rules
                                                 (filter (fn [rule]
                                                           (= (first rule) lhs))
                                                         ?rules)
                                                 specs (mapcat rest lhs-rules)
                                                 lhs-index (get symbol-table lhs)]
                                             (mapv (fn [spec]
                                                     (let [rhs (first spec)
                                                           body (rest spec)]
                                                       `(make-production '~(cons lhs spec)
                                                                         ~lhs-index
                                                                         ~(mapv symbol-table rhs)
                                                                         '(~'fn [~@(map-indexed (fn [i _]
                                                                                               (symbol (str "$" (+ 1 i))))
                                                                                             rhs)]
                                                                           ~@body))))
                                                   specs)))
                                         nonterminals)))]
    `(def ~?grammar-name
       (make-grammar '~?grammar-name
                     ~(mapv symbol-table (cons '$start nonterminals))
                     ~(mapv symbol-table (cons :$error ?terminals))
                     ~(get symbol-table :$error)
                     ~(get symbol-table '$start)
                     ~productions-by-lhs
                     '~(set/map-invert symbol-table)
                     '~symbol-table
                     ~?terminal-attribution))))

; nullable computation

(defn compute-nonterminal-nullable?
  [grammar]
  
  (let [offset (grammar-nonterminal-offset grammar)
	visited-array (boolean-array (grammar-number-of-nonterminals grammar) false)
        nullable-array (boolean-array (grammar-number-of-nonterminals grammar) false)]
    (letfn [(nullable?
              [nonterminal]
              (if (aget visited-array (- nonterminal offset))
                (aget nullable-array (- nonterminal offset))
                (do
                  (aset-boolean visited-array (- nonterminal offset) true)
                  (loop [productions (seq (grammar-productions-with-lhs nonterminal grammar))]
                    (if (empty? productions)
                      false
                      (let [production (first productions)]
                        (if (every? (fn [symbol]
                                      (and (nonterminal? symbol grammar)
                                           (nullable? symbol)))
                                    (production-rhs production))
                          (do
                            (aset-boolean nullable-array (- nonterminal offset) true)
                            true)
                          (recur (rest productions)))))))))]
      (doseq [nt (grammar-nonterminals grammar)]
        (nullable? nt))

      (fn [nt]
        (aget nullable-array (- nt offset))))))

(defn nonterminal-nullable?
  [nonterminal grammar]
  ((grammar-fetch-property grammar :nonterminal-nullable?
			   compute-nonterminal-nullable?)
   nonterminal))

(defn sequence-nullable?
  [sequence grammar]
  (not (some (fn [symbol]
	       (or (terminal? symbol grammar)
		   (not (nonterminal-nullable? symbol grammar))))
	     sequence)))

; First set computation

(defn- really-nonterminal-first
  [grammar nonterminal ^ints first-map]
  (aget first-map (- nonterminal (grammar-nonterminal-offset grammar))))

(declare restricted-append)

(defn- really-sequence-first
  [sequence k grammar nonterminal-first]
  (letfn [(recurse
            [sequence-rest]
            (if (empty? sequence-rest)
              #{'()}
              (let [cdr-first (recurse (rest sequence-rest))
                    s (first sequence-rest)]
                (if (terminal? s grammar)
                  (set
                   (map (fn [f]
                          (restricted-append k (list s) f))
                        cdr-first))
                  (apply
                   set/union
                   (map
                    (fn [f-cdr]
                      (set
                       (map
                        (fn [f-car]
                          (restricted-append k f-car f-cdr))
                        (nonterminal-first s k grammar))))
                    cdr-first))))))]
    (recurse (seq sequence))))

(defn- lhs-next-first
  [lhs k grammar old-first]
  (loop [ps (grammar-productions-with-lhs lhs grammar)
         frst '()]
    (if (empty? ps)
      frst
      (let [rhs-first
            (really-sequence-first
             (production-rhs (first ps)) k grammar
             (fn [nonterminal k grammar]
               (really-nonterminal-first grammar nonterminal old-first)))]
        (recur (rest ps)
               (set/union frst rhs-first))))))

(defn initial-first-map
  [grammar]
  ;; each nonterminal is associated with the empty set
  (object-array (repeat (grammar-number-of-nonterminals grammar) '())))

(defn next-first-map
  [grammar k last-first-map]
  ;; "Gesamtschritt" step in solving the flow equation system for first_k
  (let [new-first-map (object-array (grammar-number-of-nonterminals grammar))
	offset (grammar-nonterminal-offset grammar)]
    (doseq [nonterminal (grammar-nonterminals grammar)]
      (aset new-first-map (- nonterminal offset)
            (lhs-next-first nonterminal k grammar last-first-map)))
    new-first-map))
 
(defn map-equal?
  [fm-1 fm-2]
  (= (seq fm-1) (seq fm-2)))

(declare compute-first-1)

(defn compute-first
  [grammar k]
  (if (= 1 k)
      (compute-first-1 grammar)
      ;; fixpoint iteration
      (loop [first-map (initial-first-map grammar)]
	(let [new-first-map (next-first-map grammar k first-map)]
	  (if (not (map-equal? first-map new-first-map))
	      (recur new-first-map)
	      (fn [nonterminal]
		(really-nonterminal-first grammar nonterminal first-map)))))))

(defn nonterminal-first
  [nonterminal k grammar]
  ((grammar-fetch-property grammar [:nonterminal-first k]
			   (fn [grammar]
			     (compute-first grammar k)))
   nonterminal))

(defn sequence-first
  [sequence k grammar]
  (really-sequence-first sequence k grammar nonterminal-first))

; first_1 computation

(defn compute-first-1
  [grammar]
  (let [first-map (object-array (repeat (grammar-number-of-nonterminals grammar) '()))
	depths (int-array (grammar-number-of-nonterminals grammar) 0)
	offset (grammar-nonterminal-offset grammar)
        for-each-nonterminal (fn [f]
                               (doseq [nt (grammar-nonterminals grammar)]
                                 (f nt)))
        ;; each Y with X -> \alpha Y \beta with \alpha nullable
        initial-symbols (fn [lhs]
                          (letfn
                              [(production-loop
                                 [productions symbols]
                                 (if (not (empty? productions))
                                   (loop [rhs-rest (seq (production-rhs (first productions)))
                                          symbols symbols]
                                     (if (not (empty? rhs-rest))
                                       (let [symbol (first rhs-rest)
                                             visited? (or (= lhs symbol)
                                                          (contains? symbols symbol))
                                             symbols (if visited?
                                                       symbols
                                                       (conj symbols symbol))]
                                         (if (and (nonterminal? symbol grammar)
                                                  (nonterminal-nullable? symbol grammar))
                                           (recur (rest rhs-rest) symbols)
                                           (production-loop (rest productions) symbols)))
                                       (production-loop (rest productions) symbols)))
                                   symbols))]
                            (production-loop (seq (grammar-productions-with-lhs lhs grammar)) #{})))

        for-each-induction (fn [f lhs]
                             (doseq [symbol (filter (fn [symbol]
                                                      (nonterminal? symbol grammar))
                                                    (initial-symbols lhs))]
                               (f symbol)))

        associate-depth! (fn [nonterminal depth]
                           (aset-int depths (- nonterminal offset) depth))
    
        depth-association (fn [nonterminal]
                            (aget depths (- nonterminal offset)))

        overwrite-first! (fn [nonterminal-1 nonterminal-2]
                           (aset first-map (- nonterminal-1 offset)
                                 (aget first-map (- nonterminal-2 offset))))

        merge-firsts! (fn [lhs nonterminal]
                        (aset first-map (- lhs offset)
                              (set/union (aget first-map (- lhs offset))
                                         (aget first-map (- nonterminal offset)))))]

    (doseq [nonterminal (grammar-nonterminals grammar)]
      (let [initial (map list
                         (filter
                          (fn [symbol]
                            (terminal? symbol grammar))
                          (initial-symbols nonterminal)))]
        (aset first-map (- nonterminal offset)
              (if (nonterminal-nullable? nonterminal grammar)
                (conj initial '())
                initial))))

    (scc-union/complete-subsets! for-each-nonterminal
                                 =
                                 for-each-induction
                                 associate-depth! depth-association
                                 overwrite-first! merge-firsts!)

    (fn [nonterminal]
      (aget first-map (- nonterminal offset)))))

; Follow set computation

(defn initial-follow-map
  [grammar]
  (let [follow-map (object-array (repeat (grammar-number-of-nonterminals grammar) '()))]
    ;; start symbol must be followed by the empty string to get off the
    ;; ground
    (aget follow-map (- (grammar-start grammar) (grammar-nonterminal-offset grammar)) '(()))
    follow-map))

;;; perform
;;; follow (k, A) = follow (k, A) u U { first (k, beta follow (k, B)) | B -> alpha A beta }
;;; by iterating over the right sides of all productions, updating the
;;; follow-set as appropriate

(defn next-follow-map
  [grammar k ^objects last-follow-map]
  (let [new-follow-map (aclone last-follow-map)
	offset (grammar-nonterminal-offset grammar)]
    (grammar-for-each-production
     (fn [production]
       (let [lhs (production-lhs production)]
	 (loop [rhs-rest (seq (production-rhs production))]
	   (when (not-empty rhs-rest)
             (let [sym (first rhs-rest)]
               (if (terminal? sym grammar)
                 (recur (rest rhs-rest))
                 (let [fi-rest (sequence-first (rest rhs-rest) k grammar)
                       fo-lhs (aget new-follow-map (- lhs offset))
                       fo-sym (for [xs fi-rest
                                    ys fo-lhs]
                                (restricted-append k xs ys))]
                   (aset new-follow-map (- sym offset)
                         (set/union fo-sym 
                                    (aget new-follow-map (- sym offset))))
                   (recur (rest rhs-rest)))))))))
     grammar)
    new-follow-map))

(declare compute-follow-1)

(defn compute-follow
  [grammar k]
  (if (= 1 k)
      (compute-follow-1 grammar)
      ;; fixpoint iteration
      (let [offset (grammar-nonterminal-offset grammar)]
	(loop [^objects follow-map (initial-follow-map grammar)]
	  (let [new-follow-map (next-follow-map grammar k follow-map)]
	    (if (not (map-equal? follow-map new-follow-map))
              (recur new-follow-map)
              (fn [nonterminal]
                (aget follow-map (- nonterminal offset)))))))))

; follow_1 computation

(declare after-last memv)

(defn compute-follow-1
  [grammar]

  (let [follow-map (object-array (repeat (grammar-number-of-nonterminals grammar) '()))
	depths (int-array (grammar-number-of-nonterminals grammar) 0)
	offset (grammar-nonterminal-offset grammar)
        for-each-nonterminal (fn [f]
                               (doseq [nt (grammar-nonterminals grammar)]
                                 (f nt)))
        for-each-induction (fn [f nonterminal]
                             (doseq [lhs (grammar-nonterminals grammar)]
                               (when (some (fn [production]
                                             (if-let [rhs-rest (after-last nonterminal (production-rhs production))]
                                               (sequence-nullable? rhs-rest grammar)
                                               nil))
                                           (grammar-productions-with-lhs lhs grammar))
                                 (f lhs))))
        associate-depth! (fn [nonterminal depth]
                           (aset-int depths (- nonterminal offset) depth))
    
        depth-association (fn [nonterminal]
                            (aget depths (- nonterminal offset)))
        overwrite-follow! (fn [nonterminal-1 nonterminal-2]
                            (aset follow-map (- nonterminal-1 offset)
                                      (aget follow-map (- nonterminal-2 offset))))
        merge-follows! (fn [lhs nonterminal]
                         (aset follow-map (- lhs offset)
                               (set/union (aget follow-map (- lhs offset))
                                          (aget follow-map (- nonterminal offset)))))]

    (doseq [nonterminal (grammar-nonterminals grammar)]
      (let [follow (atom #{})]
        (grammar-for-each-production
         (fn [production]
           (if-let [rhs-rest (memv nonterminal (production-rhs production))]
             (loop [rhs-rest (seq (rest rhs-rest))]
               (if (not-empty rhs-rest)
                 (let [frst (disj (sequence-first rhs-rest 1 grammar) '())]
                   (swap! follow set/union frst)
                   (if (and (nonterminal? (first rhs-rest) grammar)
                            (nonterminal-nullable? (first rhs-rest) grammar))
                     (recur (rest rhs-rest))
                     (if-let [rhs-rest (memv nonterminal rhs-rest)]
                       (recur (rest rhs-rest)))))))))
	  grammar)
        (aset follow-map (- nonterminal offset) @follow)))

    (aset follow-map (- (grammar-start grammar) offset)
          (conj (aget follow-map (- (grammar-start grammar) offset)) '()))

    (scc-union/complete-subsets! for-each-nonterminal
                                 =
                                 for-each-induction
                                 associate-depth! depth-association
                                 overwrite-follow! merge-follows!)

    (fn [nonterminal]
      (aget follow-map (- nonterminal offset)))))

(defn nonterminal-follow
  [nonterminal k grammar]
  ((grammar-fetch-property grammar [:nonterminal-follow k]
			   (fn [grammar]
			     (compute-follow grammar k)))
   nonterminal))


; List utilities

(defn restricted-append
  [k l1 l2]
  (if (empty? l1)
    (take k l2)
    (if (zero? k)
      '()
      (cons (first l1)
            (restricted-append (- k 1)
                               (rest l1)
                               l2)))))

(defn memv
  [thing coll]
  (loop [rst (seq coll)]
    (cond
     (empty? rst) nil

     (= thing (first rst)) rst

     :else (recur (rest rst)))))

(defn after-last
  [thing coll]
  (loop [rst (seq coll) 
         tail nil]
    (if-let [res (memv thing rst)]
      (recur (rest rst) rst)
      (and tail (rest tail)))))
