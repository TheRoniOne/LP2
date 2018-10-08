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

(define (hallarPesos y nodos) ; y vale x 
  (if (eq? y 0)
      nodos
      (hallarPesos (- y 1) (cdr nodos))))

(define (hallarPesosG x y tam nodos) ; x es 0 y es 1 tam es (length nodos) se debe filtrar el resultado para eliminar los -1
  (if (eq? x (- tam 1))
      '()
      (append (hallarPesos y (car nodos)) (hallarPesosG (+ x 1) (+ y 1) tam (cdr nodos)))))

(define (bucle? listaAux nodo flag) ; flag es #f solo le interesaria el y
  (if (or (empty? listaAux) flag)
      flag
      (if (eq? (car listaAux) nodo)
          (bucle? listaAux nodo #t)
          (bucle? (cdr listaAux) nodo flag))))

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
          (append (list y) (pequeno x (+ y 1) tam nodos)))))

(define (grande x y tam nodos) ; x es 0 y es 1 tam es (length nodos)
  (if (eq? x (- tam 1))
      '()
      (cons (pequeno x y tam nodos) (grande (+ x 1) (+ y 1) tam nodos))))

(define (primInterno nodos reduc agregados aux resultado) ; agregados guarda una lista con los nodos que ya fueron conectados, aux guarda pesos
  (if (eq? (length agregados) (length nodos))
      resultado
      (primInterno)))
  (grande 0 1 (length nodos) nodos)

(define (prim nodos)
  (primInterno nodos (grande 0 1 (length nodos) nodos) '(0) (hallarPesos 1 (car nodos)) '()))

(define (kruskalInterno nodos reduc agregados pesos resultado)
  (if (eq? (length agregados) (length nodos))
      resultado
      (kruskalInterno nodos reduc (append agregados (if(bucle? agregados (calcPeso nodos (car reduc) (car (cdr reduc))) (calcMenor pesos (car pesos)))
                                                       '()
                                                       (calcMenor pesos (car pesos))
                      (remove (calcMenor pesos (car pesos)) pesos)
                      (append resultado ))))))

(define (kruskal nodos) ; calcular todos los pesos e ir cogiendo el primer menor de la lista de pesos
  (kruskalInterno nodos (grande 0 1 (length nodos) nodos) '() (filter (lambda (x) (if (eq? x -1)
                        #f
                        #t)) (hallarPesosG 0 1 (length nodos) nodos)) '()))
