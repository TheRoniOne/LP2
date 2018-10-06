#lang scheme

(define nodos (list (list -1 2 4 5)
                    (list 2 -1 3 6)
                    (list 4 3 -1 3)
                    (list 5 6 3 -1)))  ;calcular el menor de ls menores y borrar los repetidos

(define (calcPeso lista x y)
  (if (<= x (length lista))
      (if (<= y (length lista))
          (list-ref (list-ref lista x) y)
          (list-ref (list-ref lista x) (length lista)))
      (if (<= y (length lista))
          (list-ref (list-ref lista (length lista)) y)
          (list-ref (list-ref lista (length lista)) (length lista)))))

(define (visitado? pos listaVis)
  (if ()
      ()))

(define (bucle? listaAux nodo flag)
  (if (or (empty? lista) flag)
      flag
      (if (eq? (car listaAux) nodo)
          ())))

(define (calcMenor lista menor)
  (if (empty? lista)
      menor
      (if (< (car lista) menor)
          (calcMenor (cdr lista) (car lista))
          (calcMenor (cdr lista) menor))))

(define (hallarPos lista valor pos)
  (if (eq? (car lista) valor)
      pos
      (hallarPos (cdr lista) valor (+ pos 1))))

;(define lista '(2 -1 5 3))
;(hallarPos lista (calcMenor (filter positive? lista) 100) 0) ; implementacion correcta de las funciones

;(map (lambda (x) (hallarPos x (calcMenor (filter positive? x) 100) 0)) nodos) ;implementacion correcta de las funciones para cada elemento de nodos

(define (pequeno x y tam nodos) ; y es x+1 tam es (length nodos)
  (if (eq? y tam)
      '()
      (if (eq? (calcPeso nodos x y) -1)
          (pequeno x (+ y 1) tam nodos)
          (append (append (list x) (list y) (pequeno x (+ y 1) tam nodos))))))

(define (grande x y tam nodos) ; x es 0 y es 1 tam es (length nodos)
  (if (eq? x (- tam 1))
      '()
      (cons (pequeno x y tam nodos) (grande (+ x 1) (+ y 1) tam nodos))))

(define (kruskal nodos resultado)
  (if (empty? nodos)
      (resultado)
      (map (lambda (x) (hallarPos x (calcMenor (filter positive? x) 100) 0)) nodos)))

(define (prim nodos cont aux resultado)
  (if (eq? cont (cdr (calcDim nodos)))
      (resultado)
      (if (eq? cont 0)
          (prim nodos (+ cont 1) (append aux (list cont) (hallarPos (calcularMenor (car nodos) 100) 0)))
          (prim nodos (+ cont 1) (append aux (list cont) (hallarPos (calcularMenor (car nodos) 100) 0)))
