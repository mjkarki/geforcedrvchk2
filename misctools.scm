;; BSD 3-Clause License
;;
;; Copyright (c) 2018, Matti J. Kärki
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are met:
;;
;; * Redistributions of source code must retain the above copyright notice, this
;;   list of conditions and the following disclaimer.
;;
;; * Redistributions in binary form must reproduce the above copyright notice,
;;   this list of conditions and the following disclaimer in the documentation
;;   and/or other materials provided with the distribution.
;;
;; * Neither the name of the copyright holder nor the names of its
;;   contributors may be used to endorse or promote products derived from
;;   this software without specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
;; DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
;; CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
;; OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
;; OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;; port->string port
;;   reads all characters from the input port and returns them as a string

(define (port->string port)
  (list->string (read-all port read-char)))

;; log10 x
;;   returns 10 base logarithm y to the value x
;;   10^y=x

(define (log10 x)
  (/ (log x) (log 10)))

;; sort fn lst
;;   performs a merge sort for a list
;;
;;   fn - function for comparison, e.g. < or string<? for ascending sort
;;                                      > or string>? for descending sort
;;   lst - list to be sorted

(define (sort fn lst)
  (let* ((len (length lst))
         (l/2 (floor (/ len 2))))
    (cond
     ((> len 1)
      (_merge fn (sort fn (take lst l/2)) (sort fn (drop lst l/2))))
     (else
      lst))))

(define (_merge fn a b)
  (cond
   ((null? a)
    b)
   ((null? b)
    a)
   ((fn (car a) (car b))
    (cons (car a) (_merge fn (cdr a) b)))
   (else
    (cons (car b) (_merge fn a (cdr b))))))

;; search lst item
;;   find item from the list and return the index
;;
;;   lst  - a list
;;   item - item to be found, same format as the items in the list
;;
;;   return value:
;;     #f      - if the item was not found from the list
;;     integer - index of the first occurrence found from the list

(define (search lst item)
  (letrec ((s (lambda (l i n)
                (cond
                 ((null? l)
                  #f)
                 ((equal? i (car l))
                  n)
                 (else
                  (s (cdr l) i (+ n 1)))))))
    (s lst item 0)))

;; integer->hex int
;;   converts integer value to a hexadecimal string
;;
;;   int - positive integer number

(define (integer->hex int)
  (cond
   ((integer? int)
    (let ((hex (_integer->hex (abs int))))
      (cond
       ((= 0 (remainder (length hex) 2))
        (list->string (reverse hex)))
       (else
        (list->string (cons #\0 (reverse hex)))))))
   (else
    "")))

(define (_integer->hex int)
  (let ((digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
                      #\8 #\9 #\A #\B #\C #\D #\E #\F))
        (rem (remainder int 16))
        (div (truncate (/ int 16))))
    (cond
     ((= div 0)
      (list (list-ref digits (inexact->exact rem))))
     (else
      (cons (list-ref digits (inexact->exact rem))
            (_integer->hex div))))))

;; hex->int8-list hex
;;   converts hexadecimal string to a list of 8-bit integers
;;
;;   hex - arbitrary length hexadecimal string
;;
;;   return value:
;;     list - list of integers

(define (hex->int8-list hex)
  (cond
   ((= 0 (remainder (string-length hex) 2))
    (_hex->int8-list hex))
   (else
    (_hex->int8-list (string-append "0" hex)))))

(define (_hex->int8-list hex)
  (cond
   ((<= (string-length hex) 2)
    (list (hex->int8 hex)))
   (else
    (cons (hex->int8 (substring hex 0 2))
          (_hex->integer-list (substring hex 2
                                         (string-length hex)))))))

;; hex->int8 hex
;;   convert 8-bit hexadecimal string to an integer
;;
;;   hex - two character string representing a hexadecimal value

(define (hex->int8 hex)
  (cond
   ((< (string-length hex) 1)
    0)
   ((< (string-length hex) 2)
    (_h->i (string-ref hex 0)))
   (else
    (+ (* (_h->i (string-ref hex 0)) 16)
       (_h->i (string-ref hex 1))))))

(define (_h->i chr)
  (let ((digits '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7
                      #\8 #\9 #\A #\B #\C #\D #\E #\F)))
    (search digits (char-upcase chr))))
