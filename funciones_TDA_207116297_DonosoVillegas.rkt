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

;; Funciones de pertenencia TDA cardsSet:

;; Selectores TDA cardsSet:

(define get-Elements (lambda (cardsSet)
                       (car cardsSet)))

(define get-numE (lambda (cardsSet)
                       (car (cdr cardsSet))))

(define get-maxC (lambda (cardsSet)
                       (car (cdr (cdr cardsSet)))))

(define get-rndFn (lambda (cardsSet)
                       (car (cdr (cdr (cdr cardsSet))))))

;; Modificadores TDA cardsSet:

;; Otras funciones TDA cardsSet:

(define m 2147483647)
(define a 1103515245)
(define c 12345)

(define rndFn (lambda (xn)
                   (modulo (+ (* a xn) c) m)
                 )
)

(define CrearCarta1 (lambda (n i lista CrearCarta2 CrearCarta3)
                    (if (= i (+ n 2))
                        (CrearCarta2 n 1 1 '() (cons (reverse lista) '()) CrearCarta3)
                        (CrearCarta1 n (+ i 1) (cons i lista) CrearCarta2 CrearCarta3))))

(define CrearCarta2 (lambda (n i j lista lista2 CrearCarta3)
                      (cond [(= i (+ n 1)) (CrearCarta3 n 1 1 1 '() lista2)]
                            [(= j 1) (CrearCarta2 n i (+ j 1) (cons (+ (* n i) (+ j 1))(cons 1 lista)) lista2 CrearCarta3)]
                            [(not (= j (+ n 1))) (CrearCarta2 n i (+ j 1) (cons (+ (* n i) (+ j 1)) lista) lista2 CrearCarta3)]
                            [(= j (+ n 1)) (CrearCarta2 n (+ i 1) (- j  n) '() (cons (reverse lista) lista2)  CrearCarta3)])))

(define CrearCarta3 (lambda (n i j k lista lista2)
                      (cond [(= i (+ n 1)) (reverse lista2)]
                            [(= j (+ n 1)) (CrearCarta3 n (+ i 1) (- j n) k '() lista2)]
                            [(= k 1) (CrearCarta3 n i j (+ k 1) (cons (+ n 2 (* n (- k 1)) (remainder (+ (*(- i 1)(- k 1))(- j 1)) n))(cons (+ i 1) lista)) lista2)]
                            [(not (= k (+ n 1))) (CrearCarta3 n i j (+ k 1) (cons (+ n 2 (* n (- k 1)) (remainder (+ (*(- i 1)(- k 1))(- j 1)) n)) lista) lista2)]
                            [(= k (+ n 1)) (CrearCarta3 n i (+ j 1) (- k n) '() (cons (reverse lista) lista2))])))



(CrearCarta1 3 1 '() CrearCarta2 CrearCarta3)

(define CrearCartas (lambda (n) (CrearCarta1 n 1 '() CrearCarta2 CrearCarta3)))

;; EJEMPLOS DE USO DE cardsSet

(define cardsSet-1 (cardsSet (list "A" "B" "C") 2 -1 rndFn))

(define cardsSet-2 (cardsSet (list "Perro" "Gato" "Raton") 2 -1 rndFn))


 