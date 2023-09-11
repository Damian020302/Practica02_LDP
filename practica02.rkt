#lang plai
#|Integrantes:
Sánchez Pavia Ángel Gabriel
Vázquez Torrijos Damián|#

(define (any? x)
  #t)

(define-type Punto
  [punto (x number?) (y number?)])

;; Ejercicio 1.a)
(define (punto-medio p q)
  (if (and (Punto? p) (Punto? q))
      (begin
        (punto (/ (+ (punto-x p) (punto-x q)) 2) 
               (/ (+ (punto-y p) (punto-y q)) 2))
        )
      (error "parametro invalido")
      )
  )

;; Ejercicio 1.b)
(define (distancia p q)
  (if (and (Punto? p) (Punto? q))
      (begin
        (sqrt (+(expt (-(punto-x q) (punto-x p)) 2)
                (expt (-(punto-y q) (punto-y p)) 2)))
        )
      (error "parametro invalido")
      )
  )

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

;;Funcion auxiliar
;;Obtiene el valor maximo del ABB
(define (max-arbol ar)
  (type-case ArbolBinarioDeBusqueda ar
    [ArbolVacio () (error 'max-arbol "Arbol Vacio")]
    [ABB (elem izq der) (if (ArbolVacio? izq)
                                 elem
                                 (max-arbol izq))]))

;;Funcion auxiliar
;;Obtiene el valor minimo del ABB
(define (min-arbol ar)
  (type-case ArbolBinarioDeBusqueda ar
    [ArbolVacio () (error 'min-arbol "Arbol Vacio")]
    [ABB (elem izq der) (if (ArbolVacio? der)
                                 elem
                                 (min-arbol der))]))

;;Funcion auxiliar
;;Hace una lista con los valores del ABB
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
;;Funcion auxiliar
;;Elimina y cambia la raiz del ABB
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
  (define lr (getReLs ls ls))
  (define indice (mas-rep 0 0 lr lr (car lr)))
  (getE 0 indice ls))

;;funcion auxiliar (punto extra) proceso de busqueda
(define (mas-rep ie i lr lr1 r)
  [if (empty? lr1)
      ie
      {begin
        (set! r (getE 0 ie lr))
        [if (>= r (car lr1))
            {begin
              (mas-rep ie (+ 1 i) lr (cdr lr1) r)
              }
            {begin
              (set! r (getE 0 i lr)) 
              (mas-rep i (+ 1 i) lr (cdr lr1) r)
              }
            ]
        }
      ]
  
  )

;;funcion auxiliar (punto extra) obtener elemento
;;en la posición nf dada la lista ls
(define (getE ni nf ls)
  [if (empty? ls)
      (error "no existe indice")
      {begin
        [if (equal? ni nf)
            (car ls)
            (getE (+ 1 ni) nf (cdr ls))]
        }
      ] 
  )

;;función auxiliar (punto extra)
;;obtener lista de numero de veces repetidas
;;de una lista lss
(define (getReLs lsp lss)
  [if (empty? lss)
      (error "lista vacía")
      {begin
        [if (empty? (cdr lsp))
            (list (veces-repetidas lss (car lsp)))
            (cons (veces-repetidas lss (car lsp)) (getReLs (cdr lsp) lss))]
        }
      ]
  )

;;función auxiliar (punto extra)
;;dado un elemento y una lista obtener el
;;numero que se repite el elemento a
(define (veces-repetidas ls a)
  (if (empty? ls)
      0
      (if (equal? (car ls) a) 
          (+ 1 (veces-repetidas (cdr ls) a))
          (veces-repetidas (cdr ls) a))
      )
  )