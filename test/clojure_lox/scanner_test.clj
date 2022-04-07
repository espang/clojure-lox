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

(deftest testing-consume-line-comment
  (testing "should move current to the position after the comment"
    (dorun (for [[current scanner] [[2 {:content "//" :current 0}]
                                    [2 {:content "//\nvar" :current 0}]
                                    [7 {:content "abc //c" :current 4}]
                                    [7 {:content "abc //c\nvar" :current 4}]]]
             (is (= current (:current (sut/consume-line-comment scanner))))))))

(deftest testing-scan-string
  (testing ""
    (dorun  (for [[before after] [[{:content "\"\"" :current 0 :line 1}
                                   {:content "\"\"" :current 2 :line 1
                                    :tokens [(sut/make-token :token/string 1)]}]
                                  [{:content "\"\n\"" :current 0 :line 1}
                                   {:content "\"\n\"" :current 3 :line 2
                                    :tokens [(sut/make-token :token/string "\n" 2)]}]
                                  [{:content "\"hello" :current 0 :line 1}
                                   {:content "\"hello" :current 0 :line 1
                                    :errors  ["[line 1] Error: unterminated string."]}]
                                  ;; can handle escaped quotes inside the string
                                  [{:content "\"\\\"\"" :current 0 :line 1}
                                   {:content "\"\\\"\"" :current 3 :line 1
                                    :tokens [(sut/make-token :token/string "\\\"" 1)]}]]]
              (is (= (update after :content vec)
                     (sut/scan-string (update before :content vec))))))))
