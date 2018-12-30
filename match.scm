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

(define (match pattern text)
  (list->string (reverse (_match1 (string->list pattern) (string->list text)))))

(define (_match1 pattern text)
  (let ((result (_match2 pattern text #f '())))
    (cond
     ((and (null? result) (null? text))
      '())
     ((not (null? result))
      result)
     (else
      (_match1 pattern (cdr text))))))

(define (_match2 pattern text collect result)
  (cond
   ((and (null? text) (null? result))                                           ; We have parsed through the text without match -> no match
    '())
   ((null? pattern)                                                             ; We have parsed successfully through the pattern -> match found
    result)
   ((null? text)                                                                ; We have parsed successfully through the text, possibly eof? possibly partial match? return what we already might have
    result)
   ((equal? (car pattern) #\()                                                  ; Start collecting the result
    (_match2 (cdr pattern) text #t result))
   ((equal? (car pattern) #\))                                                  ; Stop collecting the result
    (_match2 (cdr pattern) text #f result))
   (collect                                                                     ; We are now collecting characters for the result
    (cond
     ((equal? (car pattern) #\N)                                                ; Start the search for numeric match
      (cond
       ((char-numeric? (car text))                                              ; Numeric character found, collect it
        (_match2 pattern (cdr text) collect (cons (car text) result)))
       (else                                                                    ; Non-numeric character found, move on with the pattern
        (_match2 (cdr pattern) text collect result))))
     ((equal? (car pattern) #\*)                                                ; Start the search for any match
      (cond
       ((and (> (length pattern) 2) (equal? (caddr pattern) (car text)))        ; Check what character should stop the matching and have we found it yet (i.e. what is the character after closing bracket) only "*)" pattern is accepted
        (_match2 (cdr pattern) text collect result))
       (else                                                                    ; Collect the character
        (_match2 pattern (cdr text) collect (cons (car text) result)))))
     ((equal? (car pattern) #\.)                                                ; Match for any single character
      (_match2 (cdr pattern) (cdr text) collect (cons (car text) result)))
     (else                                                                      ; No match (possibly eof?), stop searching
      '())))
   ((equal? (car pattern) (car text))                                           ; Pattern matches the text, proceed with the search
    (_match2 (cdr pattern) (cdr text) collect result))
   (else                                                                        ; Pattern does not match the text, stop searching
    '())))
