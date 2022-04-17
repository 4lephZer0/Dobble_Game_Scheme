#lang racket

(require "TDA_cardsSet_20711629-7_DonosoVillegas.rkt")


;; TDA Game

;; Representacion TDA Game:
;; 
;; Game va a ser un Juego de Dobble donde se puede jugar entre varios jugadores, y se va a construir en base a ciertos parametros.
;; los cuales son los siguientes numPlayers, cardsSet, mode, rndFn.
;; numPlayers: int (representa la cantidad de jugadores)
;; cardsSet: mazo de cartas (es un mazo el cual posee todas las cartas validas para el juego)
;; mode: fn (funcion de orden superior la cual posee el modo de juego que se va a jugar)
;; rndFn: fn (funcion para otorgar numeros random, los cualen pueden utilizarse para cualquier aspecto y representar transparencia referencial)

;; ------- Constructores TDA cardsSet -------

(define game (lambda (numPlayers cardsSet mode rndFn)
               (cond[(dobble? cardsSet)(list cardsSet (numCards cardsSet) '() '() numPlayers mode rndFn 0)])))

;; ------- Funciones de Pertenencia TDA cardsSet -------

(define register?(lambda (user listaRegistrados)
                   (cond [(null? listaRegistrados) #f]
                         [else
                          (cond [(eq? user (car listaRegistrados)) #t]
                                [else (register? user (cdr listaRegistrados))])])))

(define whoseTurnIsIt? (lambda(game)
                         (cond[(> (get-Turn game) 0)(whoseTurnIsIt? (list (get-cardsSet game)(get-Cards game)(cdr (get-Registers game))(get-CardsRegisters game)
                                         (get-NumPlayers game)(get-Mode game)(get-RndFn game)(- (get-Turn game) 1)))]
                              [else (car (get-Registers game))])))


;; ------- Selectores TDA cardsSet -------

(define get-cardsSet (lambda (game)
                    (car game)))

(define get-Cards (lambda (game)
                    (car(cdr game))))

(define get-Registers (lambda (game)
                    (car(cdr(cdr game)))))

(define get-CardsRegisters (lambda (game)
                    (car (cdr (cdr (cdr game))))))

(define get-NumPlayers (lambda (game)
                    (car (cdr (cdr (cdr (cdr game)))))))

(define get-Mode (lambda (game)
                    (car (cdr (cdr (cdr (cdr (cdr game))))))))

(define get-RndFn (lambda (game)
                    (car (cdr (cdr (cdr (cdr (cdr (cdr game)))))))))

(define get-Turn (lambda (game)
                    (car (cdr (cdr (cdr (cdr (cdr (cdr (cdr game))))))))))

(define stackMode(lambda (cardsSet)
                   (displayln "")
                   (displayln "################################")
                   (displayln "# Se extraen 2 cartas del mazo #")
                   (displayln "################################")
                   (displayln "")
                   (displayln (nthCard cardsSet 0))
                   (displayln (nthCard cardsSet 1))))

;; ------- Modificadores TDA cardsSet -------

(define register(lambda (user game)
                  (cond[(register? user (get-Registers game))(displayln "El Usuario a registrar ya se encuentra en nuestros registros") game]
                       [else (cond[(<= (get-NumPlayers game)(length (get-Registers game)))(displayln "No se puede registrar a mas usuarios en el sistema") game]
                                  [else
                                   (list (get-cardsSet game)(get-Cards game)(cons user (get-Registers game))(cons '()(get-CardsRegisters game))
                                         (get-NumPlayers game)(get-Mode game)(get-RndFn game)(get-Turn game))])])))

;; ------- Otras funciones TDA cardsSet -------

(define jugadores 2)
(define dobbleSet1 (cardsSet elementos numElementsPerCard -1 rndFn))
(define game1 (game 4 dobbleSet1 stackMode rndFn))
(define game2 (register "user1" game1))