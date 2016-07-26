(ns active.lawrence.grammar-test
  (:require [active.lawrence.grammar :refer :all]))

(define-grammar g00
  (:l)
  S
  ((S ((:l) $1))))

(define-grammar g08
  (:l :r)
  S
  ((S ((S T) $1)
      ((T) $1))
   (T ((:l S :r) $1)
      ((:l :r) $1))))

; Constant arithmetic expressions

(define-grammar g10
  (:+ :- :* :/ :l :r :n)
  E
  ((E ((T) $1)
      ((T :+ E) (+ $1 $3))
      ((T :- E) (- $1 $3)))
   (T ((P) $1)
      ((P :* T) (* $1 $3))
      ((P :/ T) (/ $1 $3)))
   (P ((:n) $1)
      ((:l E :r) $2))))

(define-grammar g10-error
  (:+ :- :* :/ :l :r :n)
  E
  ((E ((T) $1)
      ((:$error) 0)
      ((T :+ E) (+ $1 $3))
      ((T :- E) (- $1 $3)))
   (T ((P) $1)
      ((P :* T) (* $1 $3))
      ((P :/ T) (/ $1 $3)))
   (P ((:n) $1)
      ((:l E :r) $2)
      ((:l :$error :r) 0))))

(define-grammar g13
  (:comma :blah :dot)
  S
  ((SLK (() nil)
	((NESLK) $1))
   (NESLK ((N) $1)
	  ((NESLK K N) $1))
   (SLD (() nil)
	((NESLD) $1))
   (NESLD ((N) $1)
	  ((NESLD P N) $1))
   (S ((SLK) $1)
      ((SLD) $1))
   (K ((:comma) $1))
   (P ((:dot) $1))
   (N ((:blah) $1))))
   
;; javascript example expanded

(define-grammar g14
  (:const :comma :colon :lcurly :rcurly :lbracket :rbracket)
  S
  ((S ((E) $1))
   (E ((:const) $1)
      ((:lcurly OL :rcurly) $1)
      ((:lbracket AL :rbracket) $1))
   (C ((:comma) $1))
   (A ((:const :colon E) $1))
   (OL (() nil)
       ((ON) $1))
   (ON ((A) $1)
       ((ON C A) $1))
   (AL (() nil)
       ((AN) $1))
   (AN ((E) $1)
       ((AN C E) $1))))

(define-grammar goptional
  (:foo :bar)
  S
  ((S ((:bar O) $2))
   (O ((:foo) :present)
      (() :absent))))

