;; emacs -Q -batch -L . -l bencode-test.el -f ert-run-tests-batch

(require 'bencode)
(require 'ert)

(ert-deftest bdecode-int ()
  (should (= (bdecode-string "i512e") 512))
  (should (= (bdecode-string "i-1002e") -1002)))

(ert-deftest bdecode-string ()
  (should (string= (bdecode-string "5:hello") "hello"))
  (should (string= (bdecode-string "1: ") " "))
  (should (string= (bdecode-string "0:") "")))

(ert-deftest bdecode-list ()
  (should (equal (bdecode-string "li5ee") [5]))
  (should (equal (bdecode-string "le") []))
  (should (equal (bdecode-string "lli0eee") [[0]]))
  (should (equal (bdecode-string "l3:fooi3ee") ["foo" 3])))

(ert-deftest bdecode-dict ()
  (should (equal (bdecode-string "de") '()))
  (should (equal (bdecode-string "d3:foo3:bare") '(("foo" . "bar"))))
  (should (equal (bdecode-string "d5:helloi5ee") '(("hello" . 5))))
  (should (equal (bdecode-string "d0:d3:fooi0eee") '(("" ("foo" . 0))))))

(ert-deftest bencode--int ()
  (should (string= (bencode--int -0) "i0e"))
  (should (string= (bencode--int 102) "i102e"))
  (should (string= (bencode--int -42) "i-42e"))
  (should (string= (bencode--int 11.0) "i11e")))

(ert-deftest bencode--string ()
  (should (string= (bencode--string "hello") "5:hello"))
  (should (string= (bencode--string "") "0:"))
  (should (string= (bencode--string "❄") "3:❄")))

(ert-deftest bencode--list ()
  (should (string= (bencode--list [5 "foo"]) "li5e3:fooe"))
  (should (string= (bencode--list []) "le"))
  (should (string= (bencode--list [2 ["bar" -3]]) "li2el3:bari-3eee")))

(ert-deftest bencode--dict ()
  (should (string= (bencode--dict '(("foo" . 5) ("bar" . "baz")))
                   "d3:bar3:baz3:fooi5ee"))
  (should (string= (bencode--dict '()) "de"))
  (should (string= (bencode--dict '(("foo" . [1 2 "bar"])))
                   "d3:fooli1ei2e3:baree")))
