#lang racket

;; TDA cardsSet

;; Representacion TDA cardsSet:
;; 
;; cardsSet va a ser una funcion con 4 Elementos que la componen
;; los cuales son los siguientes Elements, numE, maxC, rndFn; donde:
;; Elements: list
;; numE: int
;; maxC: int
;; rndFn: fn

;; Constructores TDA cardsSet:

(define cardsSet (lambda (Elements numE maxC rndFn)
                   (list Elements numE maxC rndFn)))

;; Selectores TDA cardsSet:

;; Modificadores TDA cardsSet:

;; Otras funciones TDA cardsSet:

(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define rndFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                 )
)

;; EJEMPLOS DE USO DE cardsSet

(define cardsSet-1 (cardsSet (list "A" "B" "C") 2 -1 rndFn))

(define cardsSet-2 (cardsSet (list 1 2 3) 2 -1 rndFn))


