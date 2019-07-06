(include "misctools.scm")
(include "unittest.scm")

(test "Take"
      (take '(1 2 3 4 5) 3)
      '(1 2 3))

(test "Take - out of bounds"
      (take '(1 2) 3)
      '(1 2))

(test "Drop"
      (drop '(1 2 3 4 5) 3)
      '(4 5))

(test "Drop - out of bounds"
      (drop '(1 2) 3)
      '())

(test "Sort (asc) integer list"
      (sort <
            '(7 453 4 5 2 3 90 1 23 15 8 6 0))
      '(0 1 2 3 4 5 6 7 8 15 23 90 453))

(test "Sort (desc) integer list"
      (sort >
            '(7 453 4 5 2 3 90 1 23 15 8 6 0))
      '(453 90 23 15 8 7 6 5 4 3 2 1 0))

(test "Sort (asc) string list"
      (sort string<?
            '("dsfj" "vxc" "gas" "acc" "lass" "Gfd" "ACc" "Libdf"))
      '("ACc" "Gfd" "Libdf" "acc" "dsfj" "gas" "lass" "vxc"))

(test "Sort (desc) string list"
      (sort string>?
            '("dsfj" "vxc" "gas" "acc" "lass" "Gfd" "ACc" "Libdf"))
      '("vxc" "lass" "gas" "dsfj" "acc" "Libdf" "Gfd" "ACc"))

(define (l<? lst1 lst2)
  (cond
   ((< (length lst1) (length lst2))
    #t)
   ((> (length lst1) (length lst2))
    #f)
   ((and (null? lst1) (null? lst2))
    #f)
   ((and (null? lst1) (not (null? lst2)))
    #t)
   ((and (not (null? lst1)) (null? lst2))
    #f)
   ((= (car lst1) (car lst2))
    (l<? (cdr lst1) (cdr lst2)))
   ((< (car lst1) (car lst2))
    #t)
   ((> (car lst1) (car lst2))
    #f)))

(test "Sort (asc) list of lists"
      (sort l<?
            '((1 2) (1 0) (1) () (3 2) (3 1 1)))
      '(() (1) (1 0) (1 2) (3 2) (3 1 1)))
