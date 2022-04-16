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


;; ------- Constructores TDA cardsSet -------

;; Descripcion: Funcion que construye un mazo de cartas dado los parametros de entrada (lista de elementos, numero maximo de elementos, numero maximo de cartas y la funcion de randomización)
;; Dom: Elements(list) x numE(int) x maxC(int) x rndFn(fn)
;; Rec: cardsSet
;; Recursion: no aplica por encima, pero dentro del llamado a la siguiente funcion si utilizan recursion para la generacion
(define cardsSet (lambda (Elements numE maxC rndFn)
                   (CrearCarta1 (- numE 1) 1 '() Elements maxC rndFn)))


;; Descripcion: Funcion que se encarga de crear la primera carta del mazo.
;; Dom: n(int) x i(int) x lista(list) x Elements(list) x maxC(int) x rndFn(fn).
;; Rec: CrearCarta2(llamado a otra funcion para crear las siguientes n cartas del mazo).
;; Recursion: Cola, ya que no deja estados pendientes.
(define CrearCarta1 (lambda (n i lista Elements maxC rndFn)
                      (cond [(= i (+ n 2)) (CrearCarta2 n 1 1 '() (cons (reverse lista) '()) Elements maxC rndFn)]
                            [else (CrearCarta1 n (+ i 1) (cons i lista) Elements maxC rndFn)])))


;; Descripcion: Funcion que se encarga de crear las siguientes n cartas del mazo.
;; Dom: n(int) x i(int) x j(int) x lista(list) x lista2(list) x Elements(list) x maxC(int) x rndFn(fn).
;; Rec: CrearCarta3(llamado a otra funcion para crear el resto de cartas del mazo).
;; Recursion: Cola, ya que no deja estados pendientes.
(define CrearCarta2 (lambda (n i j lista lista2 Elements maxC rndFn)
                      (cond [(= i (+ n 1)) (CrearCarta3 n 1 1 1 '() lista2 Elements maxC rndFn)]
                            [(= j 1) (CrearCarta2 n i (+ j 1) (cons (+ (* n i) (+ j 1))(cons 1 lista)) lista2 Elements maxC rndFn)]
                            [(not (= j (+ n 1))) (CrearCarta2 n i (+ j 1) (cons (+ (* n i) (+ j 1)) lista) lista2 Elements maxC rndFn)]
                            [(= j (+ n 1)) (CrearCarta2 n (+ i 1) (- j  n) '() (cons (reverse lista) lista2) Elements maxC rndFn)])))


;; Descripcion: Funcion que se encarga de crear las siguientes n^2 cartas del mazo.
;; Dom: n(int) x i(int) x k(int) x lista(list) x lista2(fn) x Elements(list) x maxC(int) x rndFn(fn).
;; Rec: ChangeElements(llamado a otra funcion para cambiar los numeros creados por "Elementos").
;; Recursion: Cola, ya que no deja estados pendientes.
(define CrearCarta3 (lambda (n i j k lista lista2 Elements maxC rndFn)
                      (cond [(= i (+ n 1)) (ChangeElements (reverse lista2) Elements '() '() maxC rndFn)]
                            [(= j (+ n 1)) (CrearCarta3 n (+ i 1) (- j n) k '() lista2 Elements maxC rndFn)]
                            [(= k 1) (CrearCarta3 n i j (+ k 1) (cons (+ n 2 (* n (- k 1)) (remainder (+ (*(- i 1)(- k 1))(- j 1)) n))(cons (+ i 1) lista)) lista2 Elements maxC rndFn)]
                            [(not (= k (+ n 1))) (CrearCarta3 n i j (+ k 1) (cons (+ n 2 (* n (- k 1)) (remainder (+ (*(- i 1)(- k 1))(- j 1)) n)) lista) lista2 Elements maxC rndFn)]
                            [(= k (+ n 1)) (CrearCarta3 n i (+ j 1) (- k n) '() (cons (reverse lista) lista2) Elements maxC rndFn)])))


;; Descripcion: Funcion que se encarga de cambiar los numeros generados en una carta, por elementos correspondientes
;;              al mismo valor dentro de una lista de elementos.
;; Dom: lista(list) x Elements(list) x lista2(list) x lista3(list) x maxC(int) x rndFn(fn).
;; Rec: MaxCards(llamado a otra funcion para crear el mazo con un maximo de cartas).
;; Recursion: Cola, ya que no deja estados pendientes.
(define ChangeElements (lambda (lista Elements lista2 lista3 maxC rndFn)
                         (cond [(null? Elements) (MaxCards lista maxC '() rndFn)]
                               [(null? lista) (MaxCards (reverse lista3) maxC '() rndFn)]
                               [else (cond [(not(null?(car lista)))
                                            (ChangeElements (cons (cdar lista) (cdr lista)) Elements (Changer (- (caar lista) 1) Elements lista2) lista3 maxC rndFn)]
                                           [(null?(car lista)) (ChangeElements (cdr lista) Elements '() (cons (reverse lista2) lista3) maxC rndFn)])])))


;; Descripcion: Funcion que se encarga de ingresar un elemento en la posision num - 1 en una lista.
;; Dom: num(int) x Elements(list) x lista2(list)
;; Rec: lista2(lista con la misma cantidad de elementos que ya tenia + 1).
;; Recursion: Cola, ya que no deja estados pendientes.
(define Changer (lambda (num Elements lista2)
                  (cond [(= num 0) (cons (car Elements) lista2)]
                        [else (Changer (- num 1) (cdr Elements) lista2)])))


;; Descripcion: Funcion que crea un mazo con una cantidad maxima de cartas en el (si es negativo crea la cantidad maxima de cartas)
;; Dom: lista(list) x maxC(int) x lista2(list) x rndFn(fn)
;; Rec: cardsSet(mazo de cartas randomizado)
;; Recursion: Cola, ya que no deja estados pendientes
(define MaxCards (lambda (lista maxC lista2 rndFn)
                   (cond [(= maxC -1) (rndFn lista)]
                         [(= maxC 0) (rndFn (reverse lista2))]
                         [else (MaxCards (cdr lista) (- maxC 1) (cons (car lista) lista2) rndFn)])))

;; ------- Funciones de pertenencia TDA cardsSet -------

;; Descripcion: Funcion que verifica si un mazo de cartas es un mazo de Dobble
;; Dom: carsSet(mazo de cartas) 
;; Rec: Boolean
;; Recursion: Cola, ya que no deja estados pendientes.
(define dobble? (lambda (cardsSet)  
                  (cond [(null? cardsSet) #t]
                        [(OneElementListEqualLists? cardsSet)
                         (cond[(null? (car cardsSet)) (dobble? (cdr cardsSet))]
                              [else (cond [(inCard? (caar cardsSet) (cdar cardsSet)) #f] ;; caar es para acortar el (car (car lista)), y cdar para (cdr (car lista))
                                          [else (dobble? (cons (cdar cardsSet)(cdr cardsSet)))])])]
                        [else #t])))


;; Descripcion: Funcion que verifica si una carta, tiene un solo elemento en comun con el resto de cartas del mazo
;; Dom: carsSet (mazo de cartas)
;; Rec: Boolean
;; Recursion: Cola, ya que no deja estados pendientes.
(define OneElementListEqualLists?(lambda (cardsSet)
                                   (cond[(null? cardsSet) #t]
                                        [(null? (cdr cardsSet)) #t]
                                        [else (cond[(OneElementEqual? (car cardsSet)(cadr cardsSet) 0)(OneElementListEqualLists? (cons (car cardsSet)(cddr cardsSet)))];; cddr = (cdr(cdr lista))
                                                   [else #f])])))


;; Descripcion: Funcion que verifica si una lista (carta) tiene un unico elemento igual con otra lista (carta)
;; Dom: lista(list) x lista(list) x i (int)
;; Rec: Boolean
;; Recursion: Cola, ya que no deja estados pendientes.
(define OneElementEqual?(lambda (lista1 lista2 i)
                          (cond[(= i 2) #f]
                               [else(cond[(null? lista1)
                                          (cond[(= i 1) #t]
                                               [else #f])]
                                         [else(cond [(inCard? (car lista1) lista2) (OneElementEqual? (cdr lista1) lista2 (+ i 1))]
                                                    [else (OneElementEqual? (cdr lista1) lista2 i)])])])))


;; Descripcion: Funcion que verifica si un Elemento se encuentra en una lista / carta
;; Dom: elemento(string) x lista(list) 
;; Rec: Boolean
;; Recursion: Cola, ya que no deja estados pendientes.
(define inCard? (lambda (elemento lista)
                  (cond [(null? lista) #f]
                        [else (cond [(eq? elemento (car lista)) #t]
                                    [else (inCard? elemento (cdr lista))])])))

;; ------- Selectores TDA cardsSet -------

(define numCards (lambda (cardsSet)
                   (length cardsSet)))

(define get-firstCard (lambda (cardsSet)
                        (car cardsSet)))

(define get-SecondCard (lambda (cardsSet)
                         (car (cdr cardsSet))))

(define get-LastCard (lambda (cardsSet)
                       (car (reverse cardsSet))))

(define nthCard (lambda (cardsSet n)
                  (cond[(or (< n 0)(> n (- (length cardsSet) 1))) #f]
                       [(> n 0) (nthCard (cdr cardsSet) (- n 1))]
                       [else (car cardsSet)])))

(define findTotalCards(lambda (card)
                        (+ (* (- (length card) 1) (- (length card) 1)) (- (length card) 1) 1)))

;; ------- Modificadores TDA cardsSet -------

;; ------- Otras funciones TDA cardsSet -------

(define rndFn (lambda (lista) (reverse lista)))

;; EJEMPLOS DE USO DE cardsSet

(define cardsSet-1 (cardsSet (list "A" "B" "C") 2 -1 rndFn))

(define cardsSet-2 (cardsSet (list "Perro" "Gato" "Raton") 2 -1 rndFn))

(define cardsSet-3 (cardsSet (list "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M") 4 -1 rndFn))


 