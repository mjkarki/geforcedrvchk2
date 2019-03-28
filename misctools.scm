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

;; integer->hex
;;   converts integer value to hexadecimal string
;;
;;   int - positive integer number

(define (integer->hex int)
  (letrec* ((digits '("0" "1" "2" "3" "4" "5" "6" "7"
                      "8" "9" "A" "B" "C" "D" "E" "F"))
            (n->h (lambda (i)
                    (let ((rem (remainder i 16))
                          (div (truncate (/ i 16))))
                      (cond
                       ((= div 0)
                        (list-ref digits rem))
                       (else
                        (string-append (n->h div) (list-ref digits rem)))))))
            (hex (n->h int)))
    (cond
     ((= 0 (remainder (string-length hex) 2))
      hex)
     (else
      (string-append "0" hex)))))
