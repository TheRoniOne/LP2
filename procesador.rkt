#lang racket
(define nodos (list (list -1 2 6 5)
                    (list 2 -1 3 1)
                    (list 4 3 -1 3)
                    (list 5 6 3 -1)))

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

(define lista '(2 -1 5 3))
(hallarPos lista (calcMenor (filter positive? lista) 100) 0) ; implementacion correcta de las funciones

(map (lambda (x) (hallarPos x (calcMenor (filter positive? x) 100) 0)) nodos) ;implementacion correcta de las funciones para cada elemento de nodos
