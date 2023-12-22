(include "misctools.scm")
(include "unittest.scm")

(test "Hex to int: 8-bit value conversion"
      (hex->int8 "A1")
      161)

(test "Hex to int: 4-bit value conversion"
      (hex->int8 "C")
      12)

(test "Hex to int: empty value"
      (hex->int8 "")
      0)

(test "Find list item"
      (search '(1 2 3) 2)
      1)

(test "Find incorrect list item"
      (search '(1 2 3) 4)
      #f)

(test "Int to hex"
      (integer->hex 1234)
      "04D2")

(test "Zero to double-zero"
      (integer->hex 0)
      "00")

(test "Hex to integer list"
      (hex->int8-list "04D2")
      '(4 210))

(test "Hex to integer list with half byte"
      (hex->int8-list "4D2")
      '(4 210))

(test "4-bit value to integer"
      (hex->int8-list "4")
      '(4))

(test "Empty hex string to integer"
      (hex->int8-list "")
      '(0))
