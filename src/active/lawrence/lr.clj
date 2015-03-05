(ns active.lawrence.lr
  (:require [clojure.set :as set]
            [active.clojure.record :refer :all]
            [active.lawrence.grammar :refer :all]
            [active.lawrence.runtime :refer :all]
            [clojure.pprint :refer (pprint)]))

; LR support code needed at generation time

(declare number-coll<?)

(defn production<?
  [p1 p2]
  (number-coll<? (cons (production-lhs p1) (production-rhs p1))
		 (cons (production-lhs p2) (production-rhs p2))))

; LR items

; They must be distinguishable from pairs.

(define-record-type Item
  (make-item production position lookahead)
  item?
  [production item-production
   position item-position
   lookahead item-lookahead])

(defn predict-item?
  [item]
  (zero? (item-position item)))

(defn lookahead<?
  [la1 la2]
  (number-coll<? la1 la2))

(defn item-lhs
  [item]
  (production-lhs (item-production item)))
(defn item-rhs
  [item]
  (production-rhs (item-production item)))

(defn item-rhs-rest
  [item]
  (nthrest (production-rhs (item-production item))
           (item-position item)))

(defn item-shift
  [item]
  (make-item (item-production item)
	     (+ 1 (item-position item))
	     (item-lookahead item)))

(defn item<?
  [item-1 item-2]
  (or (production<? (item-production item-1)
		    (item-production item-2))
      (< (item-position item-1)
	 (item-position item-2))
      (lookahead<? (item-lookahead item-1) (item-lookahead item-2))))

(defn- partition-coll
  [pred lis]
  (let [mp (group-by pred lis)]
    [(mp true) (mp false)]))

(defn partition-items
  [item items]
  (let [production (item-production item)
        position (item-position item)]
    (partition-coll
     (fn [item]
       (and (= production (item-production item))
            (= position (item-position item))))
     items)))

(defn compute-lr-closure
  [state grammar k]
  (let [initial-items (fn [symbol lookahead-suffix]
                        (set
                         (mapcat (fn [production]
                                   (map
                                    (fn [la]
                                      (make-item production 0 la))
                                    (sequence-first lookahead-suffix k grammar)))
                                 (grammar-productions-with-lhs symbol grammar))))
        
        next-predict (fn [item-set-0]
                       (loop [item-set (seq item-set-0)
                              predict-set item-set-0]
                         (if (empty? item-set)
                           predict-set
                           (let [item (first item-set)
                                 rhs-rest (item-rhs-rest item)]
                             (if (empty? rhs-rest)
                               (recur (rest item-set) predict-set)
                               (let [lhs (first rhs-rest)]
                                 (if (terminal? lhs grammar)
                                   (recur (rest item-set) predict-set)
                                   (let [new-items
                                         (initial-items
                                          lhs
                                          (concat (rest rhs-rest)
                                                  (item-lookahead item)))]
                                     (recur (rest item-set)
                                            (set/union new-items predict-set))))))))))]

    (loop [predict-set state]
      (let [new-predict-set (next-predict predict-set)]
        (if (= predict-set new-predict-set)
	  predict-set
	  (recur new-predict-set))))))

(defn sequences-initial-nonterminals
  [sequences grammar]
  (map first
       (filter
        (fn [sequence]
          (and (not (empty? sequence))
               (not (terminal? (first sequence) grammar))))
        sequences)))

(defn productions->slr-predict-items
  [productions k grammar]

  (let [production->slr-predict-items (fn [production]
                                        (set
                                         (map (fn [lookahead]
                                                (make-item production 0 lookahead))
                                              (nonterminal-follow (production-lhs production) k grammar))))]
    
    (apply set/union (map production->slr-predict-items productions))))

(defn compute-slr-closure
  [state grammar k]
  (let [^objects predict-sets (grammar-fetch-property
                               grammar
                               :predict-sets
                               (fn [grammar]
                                 (object-array (grammar-number-of-nonterminals grammar))))
	offset (grammar-nonterminal-offset grammar)

        compute-predict-lhses (fn [lhs]
                                (let [already-done (atom #{})]
                                  (letfn [(recurse
                                            [lhs]
                                            (or (and (empty? @already-done)
                                                     (aget predict-sets (- lhs offset)))
                                                (if (contains? @already-done lhs)
                                                  '()
                                                  (let [lhses
                                                        (filter
                                                         (fn [lhs]
                                                           (not (contains? @already-done lhs)))
                                                         (sequences-initial-nonterminals
                                                          (map production-rhs
                                                               (grammar-productions-with-lhs lhs grammar))
                                                          grammar))]
                                                    (swap! already-done conj lhs)
                                                    (cons lhs (mapcat recurse lhses))))))]
                                    (recurse lhs))))
        get-predict-lhses (fn [lhs]
                            (or (aget predict-sets (- lhs offset))
                                (let [lhses (compute-predict-lhses lhs)]
                                  (aset predict-sets (- lhs offset) lhses)
                                  lhses)))

        initial-predict-lhses (sequences-initial-nonterminals (map item-rhs-rest state) grammar)
        ;; (dummy (begin (write initial-predict-lhses) (newline)))
        predict-lhses (set (mapcat get-predict-lhses initial-predict-lhses))
        
        ;; (dummy (begin (write predict-lhses) (newline)))
        predict-productions (mapcat (fn [lhs]
                                      (grammar-productions-with-lhs lhs grammar))
                                    predict-lhses)
        ;; (dummy (begin (write predict-productions) (newline)))
        predict-items (productions->slr-predict-items predict-productions k grammar)
        ;; (dummy (begin (write predict-items) (newline)))
        ]
    (set/union state
               predict-items)))

; Operations on LR states

(defn goto
  [state-closure symbol]
  (set
   (map item-shift
        (filter (fn [item]
                  (and (not (empty? (item-rhs-rest item)))
                       (= symbol
                          (first (item-rhs-rest item)))))
                state-closure))))

(defn active
  [state]
  (loop [item-set (seq state)
         m 0]
    (if (empty? item-set)
	m
	(recur (rest item-set)
               (long (max (item-position (first item-set)) m))))))

(defn next-symbols
  [state-closure grammar]
  (loop [item-set (seq state-closure)
         symbols #{}]
    (if (empty? item-set)
      symbols
      (let [item (first item-set)
            rhs-rest (item-rhs-rest item)]
        (recur (rest item-set)
               (if (and (not (empty? rhs-rest)))
                 (conj symbols (first rhs-rest))
                 symbols))))))

(defn next-terminals
  [state-closure grammar]
  (set/select (fn [symbol]
                (and (not (= (grammar-error grammar) symbol))
                     (terminal? symbol grammar)))
              (next-symbols state-closure grammar)))

(defn next-nonterminals
  [state-closure grammar]
  (set/select (fn [symbol]
                (nonterminal? symbol grammar))
              (next-symbols state-closure grammar)))

(defn handles-error?
  [state-closure grammar]
  (contains? (next-symbols state-closure grammar) (grammar-error grammar)))

(defn accept
  [state-closure]
  (filter (fn [item]
	    (empty? (item-rhs-rest item)))
	  state-closure))

(defn find-eoi-lookahead-item
  [accept-items]
  (first (fn [item]
	   (empty? (item-lookahead item)))
	 accept-items))

(defn initial?
  [state grammar]
  (some (fn [item]
          (= (grammar-start grammar) (item-lhs item)))
        state))


; Code generation

(def ^:dynamic *display-item-closures* false)
(def ^:dynamic *trace-states* false)

(declare trace-state check-for-reduce-reduce-conflict check-for-shift-reduce-conflict)

(defn make-lookahead-matcher
  [closure k input-name generate-matching else]
  ;; FIXME: k > 1
  (assert (= k 1)) 
  
  (let [accept-items (accept closure)
        [empty non-empty] (partition-coll (fn [item] (empty? (item-lookahead item))) 
                                          accept-items)
        empty (sort item<? empty)
        non-empty (sort item<? non-empty)
        non-empty-pairs (loop [items (seq non-empty)
                               lookaheads #{} ; case can't handle duplicates
                               cases []]
                          (if (empty? items)
                            cases
                            (let [item (first items)
                                  la (first (item-lookahead item))]
                              (if (contains? lookaheads la)
                                (recur (rest items) lookaheads cases)
                                (recur (rest items) (conj lookaheads la)
                                       (conj cases [la (generate-matching item)]))))))
        
        non-empty-case `(~'case (~'pair-token (~'first ~input-name))
                          ;; group by lookahead
                          ~@(mapcat (fn [entry]
                                      [(let [lis (map first (val entry))]
                                         (if (empty? (rest lis))
                                           (first lis)
                                           lis))
                                       (key entry)])
                                    (group-by second non-empty-pairs))
                          ~else)]
    (if (empty? empty)
      non-empty-case
      `(~'if (~'empty? ~input-name)
         ~(generate-matching (first empty))
         ~non-empty-case))))

(defn- parse-bar-name
  [id sym]
  (symbol (str "ds-parse-bar-" id "-" sym)))

(defn- parse-name
  [id]
  (symbol (str "ds-parse-" id)))

(defn make-ds-parse
  "Returns [code new-state-map todo-states]."
  [state-map-0 grammar k compute-closure state]
  (let [state-map-atom (atom state-map-0)
        todo (atom '())
        state-id (fn [state]
                   (or (get @state-map-atom state)
                       (let [next-id (count @state-map-atom)]
                         (swap! state-map-atom assoc state next-id)
                         (swap! todo conj state)
                         next-id)))
        id (state-id state)
        closure (compute-closure state grammar k)
        attribute-names (map (fn [i]
                               (symbol (str "av-" i)))
                             (range 0 (active state)))
        next-terms (next-terminals closure grammar)
        next-nonterms (next-nonterminals closure grammar)
        next-symbols (concat next-terms (next-nonterminals closure grammar))
        input-name `input#
        pair-name `pair#
        parse `(~'defn- ~(parse-name id)
                 [~@attribute-names ~input-name]
                 (~'let [reduce# (~'fn []
                                 ~(make-lookahead-matcher closure k input-name
                                                          (fn [item]
                                                            (let [rhs-length (count (item-rhs item))
                                                                  lhs (item-lhs item)
                                                                  attribution (production-attribution (item-production item))
                                                                  attribute-value `(~attribution
                                                                                    ~@(reverse (take rhs-length attribute-names)))]
                                                              (if (zero? rhs-length)
                                                                `(~(parse-bar-name id lhs) ~attribute-value ~@attribute-names ~input-name)
                                                                `(~'->RetVal ~lhs ~rhs-length ~attribute-value ~input-name))))
                                                          `(c/error '~(parse-name id) "parse error")))]
                   (~'if (~'empty? ~input-name)
                     (reduce#)
                     (~'let [~pair-name (~'first ~input-name)
                           symbol# (~'pair-token ~pair-name)]
                       (~'case symbol#
                         ~@(mapcat (fn [t]
                                     [t `(~(parse-bar-name id t)
                                          (~'pair-attribute-value ~pair-name) 
                                          ~@attribute-names
                                          (~'rest ~input-name))])
                                   next-terms)
                         (reduce#))))))
        parse-bars (map (fn [symbol]
                          (let [next-state (goto closure symbol)
                                retval-name `retval#]
                            `(~'defn- ~(parse-bar-name id symbol)
                               [av# ~@attribute-names ~input-name]
                               (~'let [~(with-meta retval-name {:tag 'RetVal})
                                     (~(parse-name (state-id next-state))
                                      av#
                                      ~@(take (- (active next-state) 1) attribute-names)
                                      ~input-name)]
                                 ~(if (empty? next-nonterms)
                                    `(~'dec-dot ~retval-name)
                                    `(~'cond
                                      (~'> (.dot ~retval-name) 1) 
                                      (~'dec-dot ~retval-name)
                        
                                      ~@(if (initial? closure grammar)
                                          [`(~'= ~(grammar-start grammar) (.-lhs ~retval-name))
                                           `(~'if (~'empty? (.-input ~retval-name))
                                              ~retval-name
                                              (c/error '~(parse-bar-name id symbol) "parse error" ~symbol))]
                                          [])

                                      :else 
                                      ;;; FIXME: optimize for when next-nonterms only has 1 element
                                      (~'case (.lhs ~retval-name)
                                        ~@(mapcat (fn [nt]
                                                    [nt
                                                     `(~(parse-bar-name id nt)
                                                       (.-attribute-value ~retval-name)
                                                       ~@attribute-names
                                                       (.-input ~retval-name))])
                                               next-nonterms))))))))
                        next-symbols)
        code (vec (cons parse parse-bars))] ; strictness because state
    [code @state-map-atom @todo]))

(defn generate-ds-parse-functions
  [grammar k compute-closure]
  (let [start-state #{(make-item (grammar-start-production grammar) 0 '())}]
    (loop [state-map {start-state 0}
           todo (list start-state)
           code []]
      (if (empty? todo)
        (do
          (when *trace-states*
            (doseq [[state index] (sort-by second state-map)]
              (let [closure (compute-closure state grammar k)
                    accept-items (accept closure)]
                (trace-state 1 closure index grammar)
                (check-for-reduce-reduce-conflict closure accept-items grammar k)
                (check-for-shift-reduce-conflict closure accept-items grammar k))))
          code)
        (let [[new-code state-map new-todos] (make-ds-parse state-map grammar k compute-closure (first todo))]
          (recur state-map
                 (concat (rest todo) new-todos)
                 (concat code new-code)))))))

(defn write-ds-parse-ns
  [grammar k method ns-name reqs writer-arg]
  (let [compute-closure (case method
                          :lr compute-lr-closure
                          :slr compute-slr-closure)
        fns (generate-ds-parse-functions grammar k compute-closure)]
    (with-open [writer (clojure.java.io/writer writer-arg)]
      (binding [*out* writer
                *print-meta* true]
        (doseq [form 
                `((~'ns ~ns-name
                    (:require 
                     [active.clojure.condition :as ~'c]
                     [active.lawrence.runtime :refer :all]
                     ~@reqs)
                    (:import [active.lawrence.runtime ~'RetVal]))
                  (~'declare ~@(map second fns))
                  ~@fns
                  ~(let [input-name `input#]
                     `(~'defn ~'parse
                        [~input-name]
                        (~'let [^active.lawrence.runtime.RetVal retval# (~(parse-name 0) ~input-name)]
                          (.-attribute-value retval#)))))]
          (prn form))))))

; Conflict handling

(defn conflict-items=?
  [item-1 item-2]
  (and (= (item-production item-1)
          (item-production item-2))
       (= (item-position item-1)
	  (item-position item-2))))

(defn conflict-items-present?
  [item-1 item-2 list]
  (some (fn [p]
	  (and (conflict-items=? item-1 (first p))
	       (conflict-items=? item-2 (first p))))
	list))

(declare display-conflict)

(defn check-for-reduce-reduce-conflict
  [closure accept-items grammar k]
  (loop [items (seq accept-items)
         done '()]
    (when-not (empty? items)
      (if-let [conflict-item (let [lookahead (item-lookahead (first items))]
                               (some (fn [item]
                                       (and (= lookahead (item-lookahead item))
                                            item))
                                     (rest items)))]
        (if (conflict-items-present? (first items) conflict-item done)
          (recur (rest items) done)
          (do
            (display-conflict "Reduce-reduce" closure (first items) conflict-item grammar)
            (recur (rest items)
                   (cons [(first items) conflict-item] done))))
        (recur (rest items) done)))))

(defn check-for-shift-reduce-conflict 
  [closure accept-items grammar k]
  (let [done (atom '())]
    (doseq [item closure]
      (let [rhs-rest (item-rhs-rest item)
            lookahead (item-lookahead item)]
        (if (and (not-empty rhs-rest)
                 (terminal? (first rhs-rest) grammar))
          (let [lookaheads (sequence-first (concat rhs-rest lookahead)
                                           k grammar)]
            (doseq [conflict-item accept-items]
              (if (and (contains? lookaheads (item-lookahead conflict-item))
                       (not (conflict-items-present? item conflict-item done)))
                (do
                  (swap! done conj [item conflict-item])
                  (display-conflict "Shift-reduce" closure item conflict-item
                                    grammar))))))))))

(defn tracing-states
  [thunk]
  (binding [*trace-states* true]
    (thunk)))

(declare display-item display-closure display-items)

(defn display-conflict
  [name closure item-1 item-2 grammar]
  (print name "conflict between items")
  (display-item item-1 grammar)
  (print " and ")
  (display-item item-2 grammar)
  (println)
  (when *display-item-closures*
    (do
      (print "State closure: ")
      (display-closure closure true grammar))))

(defn display-closure
  [closure predict? grammar]
  (let [[predict core] (partition-coll predict-item? closure)]
    (display-items core grammar)
    (when predict?
      (do
        (println "----------------")
        (display-items predict grammar)))))

(defn display-items
  [items grammar]
  (loop [items (sort-by item-lhs (seq items))]
    (when (not-empty items)
      (let [item (first items)]
        (let [[this-items other-items] (partition-coll
                                        (fn [other-item]
                                          (and (= (item-production item)
                                                  (item-production other-item))
                                               (= (item-position item)
                                                  (item-position other-item))))
                                        items)]
          (display-item item grammar)
          (println " "
                   (mapv (fn [item]
                           (map (fn [s]
                                  (grammar-symbol->name s grammar))
                                (item-lookahead item)))
                         this-items))
          (recur other-items))))))

(defn display-item
  [item grammar]
  (print (grammar-symbol->name (item-lhs item) grammar)
         "->")
  (loop [rhs-symbols (item-rhs item)
         position (item-position item)]
    (cond
     (not (empty? rhs-symbols))
     (do
       (print " ")
       (when (zero? position)
         (print ". "))
       (print (grammar-symbol->name (first rhs-symbols) grammar))
       (recur (rest rhs-symbols) (- position 1)))
     (zero? position)
     (print " ."))))

(defn trace-state
  [trace-level closure index grammar]
  (println "State " index)
  (display-closure closure true grammar))

(declare display-trace-input)

(defn trace-enter
  [trace-level closure input grammar]
  (print "Entering state")
  (display-trace-input input grammar)
  (println ":")
  (display-closure closure (>= trace-level 3) grammar))

(defn trace-reduce
  [trace-level closure nonterminal attribute-value input grammar]
  (print "Reducing with" (grammar-symbol->name nonterminal grammar))
  (display-trace-input input grammar)
  (println " yielding:")
  (println attribute-value)
  (println "after returning to:")
  (newline)
  (display-closure closure (>= trace-level 3) grammar))

(defn display-trace-input
  [input grammar]
  (print " (looking at ")
  (if (not-empty input)
    (do
      (println (grammar-symbol->name (ffirst input) grammar) "," (second (first input))))
    (print "EOF"))
  (print ")"))

(defn trace-shift
  [trace-level closure terminal grammar]
  (println "Shifting with ") (grammar-symbol->name terminal grammar))

; List utilities

(defn number-coll<?
  [ts1 ts2]
  (loop [ts1 (seq ts1)
         ts2 (seq ts2)]
    (cond 
     (empty? ts1) (not (empty? ts2))
     (empty? ts2) false
     (< (first ts1) (first ts2)) true
     (> (first ts1) (first ts2)) false
     :else (recur (rest ts1) (rest ts2)))))

