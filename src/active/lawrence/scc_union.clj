(ns active.lawrence.scc-union)

; Digraph algorithm from Pennello/DeRemer
; =======================================

; This function is very generic, imperative, kludgy,
; and it has too many arguments

(defn complete-subsets!
  [for-each-a a-equal? for-each-R
   associate-depth! depth-association
   overwrite! merge!]
  (let [stack (atom '())
        depth (atom 0)]

    ;; #f means infinity
    (defn depth-min
      [a b]
      (cond (not a) b
            (not b) a
	    :else (min a b)))

    (defn descend!
      [a]
      (swap! stack conj a)
      (swap! depth inc)
      (let [depth @depth]
	(associate-depth! a depth)
	(for-each-R
	 (fn [b]
	   (when (= 0 (depth-association b)) ; can't use zero? 'cause it may be #f
             (descend! b))
	   (associate-depth! a
			     (depth-min (depth-association a)
					(depth-association b)))
	   (merge! a b))
	 a))
      
      (when (= (depth-association a) depth)
        (loop []
          (let [top (first stack)]
            (associate-depth! top nil)
            (overwrite! top a)
            (swap! stack rest)
            (when-not (a-equal? top a)
              (recur))))))

    (for-each-a (fn [a]
		  (when (= 0 (depth-association a))
		      (descend! a))))))
