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


;; A simple pattern matching library.
;;
;; Syntax:
;;
;; All characters in the search pattern are taken as is.
;; Brackets '(' and ')' define a section, which is collected for the result.
;; Inside the brackets, '*', 'A', 'N' and '.' have a special meaning:
;; * - match and collect all characters until the end of the text or until
;;     the character immediately after the closing bracket is found.
;; A - match and collect all alphabetical character until non-alphabetical
;;     character is found.
;; N - match and collect all numerical characters (0-9) until non-numerical
;;     character is found.
;; . - match and collect any single character.
;;
;; Functions:
;;
;; (match? pattern text) checks for the text, if the pattern can be found.
;; Returns #t for a match and #f if there is no match.
;;
;; Example:
;;   (match? "(World)"   "Hello, World!") --> #t
;;   (match? "value=(N)" "value=abcd")    --> #f
;;
;; (match pattern text) searches for the pattern from the text and if the
;; match is found, returns everything marked with brackets.
;;
;; Example:
;;   (match "(World)"   "Hello, World!") --> "World"
;;   (match "value=(N)" "value=1234")    --> "1234"


(define (match? pattern text)
  (not (equal? (match pattern text) "")))

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
   ((equal? (car pattern) #\()                                                  ; Start collecting the result
    (_match2 (cdr pattern) text #t result))
   ((equal? (car pattern) #\))                                                  ; Stop collecting the result
    (_match2 (cdr pattern) text #f result))
   (collect                                                                     ; We are now collecting characters for the result
    (cond
     ((equal? (car pattern) #\A)                                                ; Start the search for alphabetic match
      (cond
       ((and (not (null? text)) (char-alphabetic? (car text)))                  ; Alphabetic character found, collect it
        (_match2 pattern (cdr text) collect (cons (car text) result)))
       (else                                                                    ; Non-alphabetic character found, move on with the pattern
        (_match2 (cdr pattern) text collect result))))
     ((equal? (car pattern) #\N)                                                ; Start the search for numeric match
      (cond
       ((and (not (null? text)) (char-numeric? (car text)))                     ; Numeric character found, collect it
        (_match2 pattern (cdr text) collect (cons (car text) result)))
       (else                                                                    ; Non-numeric character found, move on with the pattern
        (_match2 (cdr pattern) text collect result))))
     ((equal? (car pattern) #\*)                                                ; Start the search for any match
      (cond
       ((and (> (length pattern) 2) (equal? (caddr pattern) (car text)))        ; Check what character should stop the matching and have we found it yet (i.e. what is the character after closing bracket) only "*)" pattern is accepted
        (_match2 (cdr pattern) text collect result))
       ((and (= (length pattern) 2) (null? text))                               ; Special case, where the '*)' is at the end of the pattern and there is no character, which would end the matching. So we stop when we reach the end of the text
        (_match2 (cdr pattern) text collect result))
       (else                                                                    ; Collect the character
        (_match2 pattern (cdr text) collect (cons (car text) result)))))
     ((equal? (car pattern) #\.)                                                ; Match for any single character
      (_match2 (cdr pattern) (cdr text) collect (cons (car text) result)))
     ((and (not (null? text)) (equal? (car pattern) (car text)))                ; Match for a specific character
      (_match2 (cdr pattern) (cdr text) collect (cons (car text) result)))
     (else                                                                      ; No match, stop searching
      '())))
   ((and (not (null? text)) (equal? (car pattern) (car text)))                  ; Pattern matches the text, proceed with the search
    (_match2 (cdr pattern) (cdr text) collect result))
   (else                                                                        ; Pattern does not match the text, stop searching
    '())))
