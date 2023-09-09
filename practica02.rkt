#lang plai

(define (any? x)
  #t)

(define-type Punto
  [punto (x number?) (y number?)])

;; Ejercicio 1.a)
(define (punto-medio p q)
  (error 'punto-medio "Sin implementar"))

;; Ejercicio 1.b)
(define (distancia p q)
  (error 'distancia "Sin implementar"))

(define-type Lista
    [Vacia]
    [Cons (cabeza any?) (resto Lista?)])

;; Funciones auxiliares Lista
(define (foldl-lista f acc ls)
  (type-case Lista ls
    [Vacia () acc]
    [Cons (cabeza resto) (foldl-lista f (f cabeza acc) resto)]))

(define (foldr-lista f acc ls)
  (type-case Lista ls
    [Vacia () acc]
    [Cons (cabeza resto) (f cabeza (foldr-lista f acc resto))]))


;; Ejercicio 2.a)
(define (longitud ls)
  (type-case Lista ls
    [Vacia () 0]
    [Cons (cabeza resto) (+ 1 (longitud resto))]))

;; Ejercicio 2.b)
(define (pertenece? e ls)
  (type-case Lista ls
    [Vacia () #f]
    [Cons (cabeza resto) (if (= cabeza e)
          #t
          (pertenece? e resto))]))

;; Ejercicio 2.c)
(define (intercala ls ks)
  (type-case Lista ls
    [Vacia () ls]
    [Cons (cabeza resto) (Cons cabeza(intercala ks resto))]))

;; Ejercicio 2.d)
(define (aplana ls)
  (type-case Lista ls
    [Vacia () '()]
    [Cons (cabeza resto) (aplana cabeza)(aplana resto)]))

(define-type ArbolBinarioDeBusqueda
    [ArbolVacio]
    [ABB (elemento number?)
         (izq ArbolBinarioDeBusqueda?)
         (der ArbolBinarioDeBusqueda?)])

;; Funciones auxiliares tipo ArbolBinarioDeBusqueda
(define (max-arbol ar)
  (type-case ArbolBinarioDeBusqueda ar
    [ArbolVacio () (error 'max-arbol "Arbol Vacio")]
    [ABB (elem izq der) (if (ArbolVacio? izq)
                                 elem
                                 (max-arbol izq))]))

(define (min-arbol ar)
  (type-case ArbolBinarioDeBusqueda ar
    [ArbolVacio () (error 'min-arbol "Arbol Vacio")]
    [ABB (elem izq der) (if (ArbolVacio? der)
                                 elem
                                 (min-arbol der))]))
(define (lista-ordenada ar)
  (type-case ArbolBinarioDeBusqueda ar
    [ArbolVacio () ""]
    [ABB (elem izq der) (string-append(lista-ordenada izq)" "(~a elem)
                                      " "(lista-ordenada der))]))

;; Ejercicio 3.a)
(define (elimina ar e)
  (type-case ArbolBinarioDeBusqueda ar
    [ArbolVacio () ar]
    [ABB (elem izq der) (cond
                          [(= e elem) elimina-raiz ar]
                          [(< e elem) (ABB (elem)(elimina izq e)(der))]
                          [else (ABB (elem)(izq)(elimina der e))])]))

(define (elimina-raiz ar)
  (type-case ArbolBinarioDeBusqueda ar
    [ArbolVacio () ar]
    [ABB (elem izq der) (cond
                          [(and (ArbolVacio? izq)(ArbolVacio? der)) ArbolVacio]
                          [(ArbolVacio? izq) der]
                          [(ArbolVacio? der) izq]
                          [else (let* ((reemplazo
                                        (elem(min-arbol der)))
                                       (n-der (elimina der reemplazo)))
                                  (ABB (reemplazo)(izq)(n-der)))])]))

;; Ejercicio 3.b)
(define (mapea-arbol ab f)
  (type-case ArbolBinarioDeBusqueda ab
    [ArbolVacio () ab]
    [ABB (elem izq der) (ABB (f elem) (mapea-arbol izq f)
                                  (mapea-arbol der f))]))

;; Ejercicio 3.c)
(define (hojas ar)
  (type-case ArbolBinarioDeBusqueda ar
    [ArbolVacio () '()]
    [ABB (elem izq der) (if(and (ArbolVacio? izq)(ArbolVacio? der))
                           (list elem)
                           (append (hojas izq)(hojas der)))]))

;; Punto Extra
(define (mas-repetido ls)
  (error 'mas-repetido "Sin implementar"))
