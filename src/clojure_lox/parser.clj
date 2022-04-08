(ns clojure-lox.parser
  (:require [clojure.string :as str]))


;; Expressions are:
;;  literals
;;  unary expression
;;  binary expression
;;  parentheses

(defn make-literal [tk]
  {:expr-type :expr/literal
   :value tk})

(defn make-grouping [expr]
  {:expr-type :expr/grouping
   :expression expr})

(defn make-unary [operator right]
  {:expr-type :expr/unary
   :operator operator
   :right right})

(defn make-binary [left operator right]
  {:expr-type :expr/binary
   :left left
   :operator operator
   :right right})

(defmulti printer :expr-type)

(defn parenthesize [s & exprs]
  (str "("
       s
       " "
       (str/join " "
                 (map printer exprs))
       ")"))

(defmethod printer :expr/binary
  [{:keys [left right operator]}]
  (parenthesize (:lexeme operator)
                left
                right))

(defmethod printer :expr/grouping
  [{:keys [expression]}]
  (parenthesize "group" expression))

(defmethod printer :expr/literal
  [{:keys [value]}]
  (if (nil? value)
    "nil"
    (:lexeme value)))

(defmethod printer :expr/unary
  [{:keys [right operator]}]
  (parenthesize (:lexeme operator)
                right))



;; --- Parser ---

(defn parser [tokens]
  {:tokens  tokens
   :current 0})

(defn is-at-end? [{:keys [current tokens]}]
  (boolean
   (or (>= current (count tokens))
       (= :token/eof (nth tokens current)))))

(defn next-token [{:keys [current tokens] :as pa}]
  (if (is-at-end? pa)
    {:token-type :token/eof}
    (nth tokens current)))

(defn next-token-matches-any
  "returns false when parser at or behind the last EOF token."
  [pa token-types]
  (boolean
   (some #{(:token-type (next-token pa))}
         token-types)))

(defn advance [pa]
  (let [tk (next-token pa)]
    [tk (update pa :current inc)]))

(defn first-expr [self f pa]
  (let [[expr pa'] (f pa)]
    (self pa' expr)))

(defn next-exprs [self f pa expr & tokens]
  (if (next-token-matches-any pa tokens)
     (let [[operator pa'] (advance pa)
           [right pa'']   (f pa')]
       (self pa'' (make-binary expr operator right)))
     [expr pa]))

(declare expression)

;; TODO: error handling in the inner case!!!
(defn primary [pa]
  (let [[{:keys [token-type lexeme]} pa'] (advance pa)]
    (case token-type
      :token/false  [(make-literal false) pa']
      :token/true   [(make-literal true) pa']
      :token/nil    [(make-literal nil) pa']
      :token/number [(make-literal lexeme) pa']
      :token/string [(make-literal lexeme) pa']
      :token/left-paren
      (let [[expr pa''] (expression pa')
            [closing pa'''] (advance pa'')]
        (case closing
          :token/right-paren
          [(make-grouping expr) pa'''])))))

(defn unary [pa]
  (if (next-token-matches-any pa [:token/bang :token/minus])
    (let [[operator pa'] (advance pa)
          [right pa'']   (unary pa')]
      [(make-unary operator right) pa''])
    (primary pa)))

(defn factor
  ([pa] (first-expr factor unary pa))
  ([pa expr]
   (next-exprs factor unary pa expr
               :token/slash
               :token/star)))

(defn term
  ([pa] (first-expr term factor pa))
  ([pa expr]
   (next-exprs term factor pa expr :token/minus :token/plus)))

(defn comparison
  ([pa] (first-expr comparison term pa))
  ([pa expr]
   (next-exprs comparison term pa expr
               :token/greater
               :token/greater-equal
               :token/less
               :token/less-equal)))

(defn equality 
  ([pa] (first-expr equality comparison pa))
  ([pa expr]
   (next-exprs equality comparison pa expr
               :token/bang-equal
               :token/equal-equal)))

(defn expression [pa] (equality pa))

(defn parse [pa]
  )

;; --- Parser ---



(comment
  (def lit (make-literal {:token-type :token/string
                          :lexeme "hello"}))
  
  (def unary-expr
    (make-unary {:type :expr/bang
                :lexeme "!"}
                lit))

  (def grouping
    (make-grouping lit))

  (def binary-expr
    (make-binary lit
                 {:type :token/and
                  :lexeme "and"}
                 unary-expr))

  (def example
    (make-binary (make-unary {:token-type :token/minus
                              :lexeme "-"}
                             (make-literal {:token-type :token/number
                                            :lexeme 123.0}))
                 {:token-type :token/star
                  :lexeme "*"}
                 (make-grouping (make-literal {:token-type :token/number
                                               :lexeme  45.67}))))

  (printer lit)
  (printer unary-expr)
  (printer grouping)
  (printer binary-expr)
  (printer example))
