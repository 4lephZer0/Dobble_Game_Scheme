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

(define MaxCards (lambda (lista maxC lista2 rndFn)
                       (cond [(= maxC -1) (rndFn lista)]
                             [(not(= maxC 0)) (MaxCards (cdr lista) (- maxC 1) (cons (car lista) lista2) rndFn)]
                             [(= maxC 0) (rndFn (reverse lista2))])))

(define ChangeElements (lambda (lista Elements lista2 lista3 Changer maxC rndFn)
                         (cond [(null? Elements) (MaxCards lista maxC '() rndFn)]
                               [(null? lista) (MaxCards (reverse lista3) maxC '() rndFn)]
                               [(not(null? lista))
                                (cond [(not(null?(car lista))) (ChangeElements (cons (cdr (car lista)) (cdr lista)) Elements (Changer (- (car (car lista)) 1) Elements lista2) lista3 Changer maxC rndFn)]
                                      [(null?(car lista)) (ChangeElements (cdr lista) Elements '() (cons (reverse lista2) lista3) Changer maxC rndFn)])])))

(define Changer (lambda (num Elements lista2)
                         (cond [(not(= num 0)) (Changer (- num 1) (cdr Elements) lista2)]
                               [(= num 0) (cons (car Elements) lista2)])))

(define CrearCarta1 (lambda (n i lista CrearCarta2 CrearCarta3 Elements maxC rndFn)
                    (if (= i (+ n 2))
                        (CrearCarta2 n 1 1 '() (cons (reverse lista) '()) CrearCarta3 Elements maxC rndFn)
                        (CrearCarta1 n (+ i 1) (cons i lista) CrearCarta2 CrearCarta3 Elements maxC rndFn))))

(define CrearCarta2 (lambda (n i j lista lista2 CrearCarta3 Elements maxC rndFn)
                      (cond [(= i (+ n 1)) (CrearCarta3 n 1 1 1 '() lista2 Elements maxC rndFn)]
                            [(= j 1) (CrearCarta2 n i (+ j 1) (cons (+ (* n i) (+ j 1))(cons 1 lista)) lista2 CrearCarta3 Elements maxC rndFn)]
                            [(not (= j (+ n 1))) (CrearCarta2 n i (+ j 1) (cons (+ (* n i) (+ j 1)) lista) lista2 CrearCarta3 Elements maxC rndFn)]
                            [(= j (+ n 1)) (CrearCarta2 n (+ i 1) (- j  n) '() (cons (reverse lista) lista2)  CrearCarta3 Elements maxC rndFn)])))

(define CrearCarta3 (lambda (n i j k lista lista2 Elements maxC rndFn)
                      (cond [(= i (+ n 1)) (ChangeElements (reverse lista2) Elements '() '() Changer maxC rndFn)]
                            [(= j (+ n 1)) (CrearCarta3 n (+ i 1) (- j n) k '() lista2 Elements maxC rndFn)]
                            [(= k 1) (CrearCarta3 n i j (+ k 1) (cons (+ n 2 (* n (- k 1)) (remainder (+ (*(- i 1)(- k 1))(- j 1)) n))(cons (+ i 1) lista)) lista2 Elements maxC rndFn)]
                            [(not (= k (+ n 1))) (CrearCarta3 n i j (+ k 1) (cons (+ n 2 (* n (- k 1)) (remainder (+ (*(- i 1)(- k 1))(- j 1)) n)) lista) lista2 Elements maxC rndFn)]
                            [(= k (+ n 1)) (CrearCarta3 n i (+ j 1) (- k n) '() (cons (reverse lista) lista2) Elements maxC rndFn)])))

(define cardsSet (lambda (Elements numE maxC rndFn) (CrearCarta1 (- numE 1) 1 '() CrearCarta2 CrearCarta3 Elements maxC rndFn)))


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

(define rndFn (lambda (lista) (reverse lista)))

(define elementos (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"))

;; EJEMPLOS DE USO DE cardsSet

(define cardsSet-1 (cardsSet (list "A" "B" "C") 2 -1 rndFn))

(define cardsSet-2 (cardsSet (list "Perro" "Gato" "Raton") 2 -1 rndFn))


 