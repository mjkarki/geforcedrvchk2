(include "match.scm")
(include "unittest.scm")

(test-equal (match "(..)34"         "123456")                      "12")
(test-equal (match "12(..)56"       "123456")                      "34")
(test-equal (match "34(..)"         "123456")                      "56")
(test-equal (match "12(..)6"        "123456")                      "")
(test-equal (match "(..)45"         "123456")                      "23")
(test-equal (match "..(..)56"       "123456")                      "")
(test-equal (match "4(N.N.)"        "aaa123456,78bbb")             "56,78b")
(test-equal (match "value: \"(N)\"" "This is the value: \"123\".") "123")
(test-equal (match "text: (*)"     "text: abc 123.")               "abc 123.")
(test-equal (match "text: (*)."     "text: abc 123.")              "abc 123")
