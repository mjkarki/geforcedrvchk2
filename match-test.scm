(include "match.scm")
(include "unittest.scm")

(test "Simple strings, identical query"
      (match "123456"
        "123456")
      "")
(test "Empty string, empty query string"
      (match ""
        "")
      "")
(test "Empty query string, simple string"
      (match ""
        "123456")
      "")
(test "Simple query, empty string"
      (match "123456"
        "")
      "")
(test "Nonmatching query, simple string"
      (match "123456"
        "654321")
      "")
(test "Matching group"
      (match "(1234)"
        "1234")
      "1234")
(test "Non-matching group"
      (match "(1234)"
        "4321")
      "")
(test "Group matches substring at the start of the string"
      (match "(..)34"
        "123456")
      "12")
(test "Group matches substring in the middle of the string"
      (match "12(..)56"
        "123456")
      "34")
(test "Group matches substring at the end of the string"
      (match "34(..)"
        "123456")
      "56")
(test "Matching group surrounded by non-matching query"
      (match "12(..)6"
        "123456")
      "")
(test "Matching group with partially matching query"
      (match "(..)45"
        "123456")
      "23")
(test "Periods do not have special meaning outside the group"
      (match "..(..)56"
        "123456")
      "")
(test "Group matches alphabetic and numeric query"
      (match "(A N)"
        "abc 123")
      "abc 123")
(test "Group matches substring and numeric query"
      (match "a(bc N)x"
        "abc 123x")
      "bc 123")
(test "Group matches but the rest of the query does not match"
      (match "a(bc N)4"
        "abc 1234")
      "")
(test "Group matches substring and anything"
      (match "a(bc *)4"
        "abc 1234")
      "bc 123")
(test "Group matches but the rest of the query does not match"
      (match "a(bc A)z"
        "abc xyz")
      "")
(test "Group and query match the string"
      (match "a(bc *)z"
        "abc xyz")
      "bc xy")
(test "Group is able to separate numbers inside and outside of the group"
      (match "4(N.N.)"
        "aaa123456,78bbb")
      "56,78b")
(test "Group picks numbers inside the double-qoutes"
      (match "value: \"(N)\""
        "This is the value: \"123\".")
      "123")
(test "Group matches correctly anything until at the end of the string"
      (match "text: (*)"
        "text: abc 123.")
      "abc 123.")
(test "Group matches correctly anything until the last character at the end of the string"
      (match "text: (*)."
        "text: abc 123.")
      "abc 123")
(test "Group picks a string correctly"
      (match "\"(A)\" is my name."
        "Hello, \"Frank\" is my name.")
      "Frank")
(test "Query handles extra space correctly"
      (match "\"(A)\" is my name."
        "Hello, \"Fra nk\" is my name.")
      "")
(test "Query handles space inside the group"
      (match "\"(A A)\" is my name."
        "Hello, \"Fra nk\" is my name.")
      "Fra nk")
(test "Combination of rules works inside the group"
      (match "start: (AN*)"
        "start: ABC123xyz")
      "ABC123xyz")
(test "Two groups are handled correctly returning a single string"
      (match "(A)123(A)"
        "abc123def")
      "abcdef")

(test "Test that match? returns a boolean true"
      (match? "(abc)"
              "abc")
      #t)
(test "Test that match? returns a boolean false"
      (match? "(abc)"
              "cba")
      #f)
(test "match? handles matching group rule"
      (match? "a = (N)"
              "a = 1")
      #t)
(test "match? handles non-matching group rule"
      (match? "a = (N)"
              "a = b")
      #f)
