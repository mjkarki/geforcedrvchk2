; test
;   description - a short description of the test case
;   expression  - expression to be tested
;   result      - expected result
(define-macro (test description expression result)
  `(cond ((equal? ,expression ,result)
          (display "OK    : ")
          (display ,description)
          (newline))
         (else
          (display "ERROR : ")
          (display ,description)
          (display " :: ")
          (display ',expression)
          (display " :: Expected ")
          (write ,result)
          (display ", got ")
          (write ,expression)
          (newline))))

