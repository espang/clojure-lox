(ns clojure-lox.scanner-test
  (:require
   [clojure.test :refer :all]
   [clojure-lox.scanner :as sut]))

(deftest testing-char-at
  (testing "returns the character at the given position or eof"
    (is (= sut/eof (sut/char-at nil 0)))
    (is (= sut/eof (sut/char-at [] 0)))
    (is (= sut/eof (sut/char-at "" 0)))
    (is (= \a (sut/char-at (vec "a") 0)))
    (is (= \a (sut/char-at "a" 0)))
    (is (= \a (sut/char-at "ab" 0)))
    (is (= \b (sut/char-at "ab" 1)))
    (is (= sut/eof (sut/char-at "ab" 2)))))

(deftest testing-scan-line-comment
  (testing "should move current to the position after the comment"
    (are [result from content] (= result (sut/scan-line-comment from (vec content)))
      [2 "//"] 0 "//"
      [2 "//"] 0 "//\nvar"
      [7 "//c"] 4 "abc //c"
      [7 "//c"] 4 "abc //c\nvar")))

(deftest testing-scan-string
  (testing "should move current to the position after the string and return the string"
    (are [result from content] (= result (sut/scan-string from (vec content)))
      [:ok 2 "" 0] 0 "\"\""
      [:ok 5 "b\na" 1] 0 "\"b\na\""
      [:error/unterminated-string] 0 "\"h")))

(deftest testing-scan-number
  (testing "should move current to the position after the number and return the number"
    (are [result from content] (= result (sut/scan-number from (vec content)))
      [1 1.0] 0 "1"
      [1 1.0] 0 "1."
      [3 1.0] 0 "1.0"
      [9 1.0] 8 "var x = 1;")))

(deftest testing-scan-identifier
  (testing "should move current to the position after the identifier and return the type and lexeme"
    (are [result from content] (= result (sut/scan-identifier from (vec content)))
      [1 "a"] 0 "a"
      [1 "a"] 0 "a "
      [3 "var"] 0 "var x = 1;"
      [5 "x"] 4 "var x= 1;")))

(deftest testing-scan-next-token
  (testing "should return the next token with some extra information"
    (let [line "var x = 1;"]
      (are [result from content] (= (merge {:after (inc from)
                                            :span-lines 0}
                                           result)
                                    (sut/scan-next-token from (vec content)))
        {:token :token/eof} 0 ""
        {:token :token/left-paren} 0 "("
        {:token :token/var :value "var" :after 3} 0 line
        {:token :ignore} 3 line
        {:token :token/identifier :value "x" :after 5} 4 line
        {:token :token/equal} 6 line
        {:token :token/number :value 1.0 :after 9} 8 line
        {:token :token/semicolon} 9 line
        {:token :token/eof} 10 line))))

(defn remove-nil-vals [m]
  (into {} (remove (comp nil? val) m)))

(deftest testing-scan-tokens
  (testing "a simple one line program"
    (is (=  [{:token-type :token/var :lexeme "var" :line 1}
             {:token-type :token/identifier :lexeme "x" :line 1}
             {:token-type :token/equal :line 1}
             {:token-type :token/number :lexeme 1.0 :line 1}
             {:token-type :token/semicolon :line 1}
             {:token-type :token/eof :line 1}] 
            (->> (sut/scan-tokens (sut/scanner "var x = 1;"))
                 :tokens
                 (map remove-nil-vals)))))
  (testing "a bit more complex program"
    (let [program "
                  // cat
                  class Cat {
                    noise() {
                      return \"miau\";
                    }
                  }

                  // dog; 
                  class Dog {
                    makeNoise() {
                      print \"bark!\";
                    }
                  }"]
      (is (= [{:token-type :token/class :lexeme "class", :line 3}          
              {:token-type :token/identifier :lexeme "Cat", :line 3}
              {:token-type :token/left-brace :line 3}
              {:token-type :token/identifier :lexeme "noise", :line 4}
              {:token-type :token/left-paren :line 4}
              {:token-type :token/right-paren :line 4}
              {:token-type :token/left-brace :line 4}
              {:token-type :token/return :lexeme "return" :line 5}
              {:token-type :token/string :lexeme "miau" :line 5}
              {:token-type :token/semicolon :line 5}
              {:token-type :token/right-brace :line 6}
              {:token-type :token/right-brace :line 7}
              {:token-type :token/class :lexeme "class" :line 10}
              {:token-type :token/identifier :lexeme "Dog" :line 10}
              {:token-type :token/left-brace :line 10}
              {:token-type :token/identifier :lexeme "makeNoise" :line 11}
              {:token-type :token/left-paren :line 11}
              {:token-type :token/right-paren :line 11}
              {:token-type :token/left-brace :line 11}
              {:token-type :token/print :lexeme "print" :line 12}
              {:token-type :token/string :lexeme "bark!" :line 12}
              {:token-type :token/semicolon :line 12}
              {:token-type :token/right-brace :line 13}
              {:token-type :token/right-brace :line 14}
              {:token-type :token/eof :line 14}]
             (->> (sut/scan-tokens (sut/scanner program))
                  :tokens
                  (map remove-nil-vals)))))))
