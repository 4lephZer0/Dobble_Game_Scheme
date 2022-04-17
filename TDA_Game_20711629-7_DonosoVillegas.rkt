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

