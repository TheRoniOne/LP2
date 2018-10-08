#lang scheme

(define nodos (list (list -1 2 4 5)
                    (list 2 -1 3 6)
                    (list 4 3 -1 3)
                    (list 5 6 3 -1)))  ;eliminar la redundancia

(define (calcPeso lista x y)
  (if (<= x (length lista))
      (if (<= y (length lista))
          (list-ref (list-ref lista x) y)
          (list-ref (list-ref lista x) (length lista)))
      (if (<= y (length lista))
          (list-ref (list-ref lista (length lista)) y)
          (list-ref (list-ref lista (length lista)) (length lista)))))

(define (hallarPesos y nodos) ; y vale x (para kruskal)
  (if (eq? y 0)
      nodos
      (hallarPesos (- y 1) (cdr nodos))))

(define (hallarPesosG x y tam nodos) ; x es 0 y es 1 tam es (length nodos) se debe filtrar el resultado para eliminar los -1 (funcion optimizada para kruskal)
  (if (eq? x (- tam 1))
      '()
      (append (hallarPesos y (car nodos)) (hallarPesosG (+ x 1) (+ y 1) tam (cdr nodos)))))

(define (presente? nodo lista flag)
  (if (or (empty? lista) flag)
      flag
      (if (eq? (car (list lista)) nodo)
          #t
          (presente? nodo (cdr lista) flag))))

(define (bucle? listaAux nodo1 nodo2) 
  (if (and (presente? nodo1 listaAux #f) (presente? nodo2 listaAux #f))
      #t
      #f))

(define (calcMenor lista menor)
  (if (empty? lista)
      menor
      (if (< (car lista) menor)
          (calcMenor (cdr lista) (car lista))
          (calcMenor (cdr lista) menor))))

(define (buscarPos x y tam valor reduc nodos) ; y es x+1 tam es (length nodos) (falta arreglar)
  (if (eq? y tam)
      '()
      (if (and (eq? (calcPeso nodos x y) valor) (presente? y reduc #f))
          (append (list x)(list y) (buscarPos x (+ y 1) tam valor reduc nodos))
          (buscarPos x (+ y 1) tam valor reduc nodos))))
          
(define (buscarPosG x y tam valor reduc nodos) ; x es 0 y es 1 tam es (length nodos) (falta arreglar)
  (if (eq? x (- tam 1))
      '()
      (append (buscarPos x y tam valor (car reduc) nodos) (buscarPosG (+ x 1) (+ y 1) tam valor (cdr reduc) nodos))))

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

(define (prim nodos)
  (primInterno nodos (grande 0 1 (length nodos) nodos) '(0) (filter (lambda (x) (if (eq? x -1)
                        #f
                        #t)) (hallarPesos 1 (car nodos))) '()))

(define (primInterno nodos reduc agregados pesos resultado) ; agregados guarda una lista con los nodos que ya fueron conectados, aux guarda pesos
  (if (eq? (length agregados) (length nodos))
      resultado
      (primInterno nodos (remove (car (cdr (buscarPosG 0 1 (length nodos) (calcMenor pesos (car pesos)) reduc nodos))) (list-ref reduc (car (buscarPosG 0 1 (length nodos) (calcMenor pesos (car pesos)) reduc nodos))))
                   (append agregados (if (bucle? agregados (car (buscarPosG 0 1 (length nodos) (calcMenor pesos (car pesos)) reduc nodos)) (car (cdr(buscarPosG 0 1 (length nodos) (calcMenor pesos (car pesos)) reduc nodos)))) ; arreglar reducido
                                                       '()
                                                       (car (cdr(buscarPosG 0 1 (length nodos) (calcMenor pesos (car pesos)) nodos)))))
                                       ;()
                                       (append resultado (if (bucle? agregados (car (cdr(buscarPosG 0 1 (length nodos) (calcMenor pesos (car pesos)) reduc nodos)))) ; arreglar reducido
                                                       '()
                                                       (append (car (buscarPosG 0 1 (length nodos) (calcMenor pesos (car pesos)) reduc nodos)) (car (cdr (buscarPosG 0 1 (length nodos) (calcMenor pesos (car pesos)) reduc nodos))) (calcMenor pesos (car pesos))))))))

(define (kruskal nodos) ; calcular todos los pesos e ir cogiendo el primer menor de la lista de pesos
  (kruskalInterno nodos (grande 0 1 (length nodos) nodos) '() (filter (lambda (x) (if (eq? x -1)
                        #f
                        #t)) (hallarPesosG 0 1 (length nodos) nodos)) '()))

(define (kruskalInterno nodos reduc agregados pesos resultado)
  (if (eq? (length agregados) (length nodos))
      resultado
      (kruskalInterno nodos (remove (list-ref (buscarPosG 0 1 (length nodos) (calcMenor pesos (car pesos)) reduc nodos) 1) (list-ref reduc (car (buscarPosG 0 1 (length nodos) (calcMenor pesos (car pesos)) reduc nodos))))
                      (append agregados (if (bucle? agregados (car (buscarPosG 0 1 (length nodos) (calcMenor pesos (car pesos)) reduc nodos)) (car (cdr (buscarPosG 0 1 (length nodos) (calcMenor pesos (car pesos)) reduc nodos))))
                                                       '()
                                                       (list (car (cdr (buscarPosG 0 1 (length nodos) (calcMenor pesos (car pesos)) reduc nodos))))))
                      (remove (calcMenor pesos (car pesos)) pesos)
                      (append resultado (if (bucle? agregados (car (buscarPosG 0 1 (length nodos) (calcMenor pesos (car pesos)) reduc nodos)) (car (cdr (buscarPosG 0 1 (length nodos) (calcMenor pesos (car pesos)) reduc nodos))))
                                                       '()
                                                       (append (list (car (buscarPosG 0 1 (length nodos) (calcMenor pesos (car pesos)) reduc nodos)))
                                                               (list (car (cdr (buscarPosG 0 1 (length nodos) (calcMenor pesos (car pesos)) reduc nodos))))
                                                               (list (calcMenor pesos (car pesos)))))))))

