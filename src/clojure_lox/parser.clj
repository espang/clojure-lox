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

(defn next-token-matches-any [{:keys [tokens current]} & tokens]
  (boolean
   (some #{(:token-type (nth tokens current {}))}
         tokens)))

(defn advance [{:keys [current tokens] :as pa}]
  (let [tk (nth tokens current)]
    [tk (update pa :current inc)]))

(defn equality 
  ([pa]
   (let [[expr pa'] (comparison pa)]
     (equality pa' expr)))
  ([pa expr]
   (if (next-token-matches-any pa :token/bang-equal :token/equal-equal)
     (let [[operator pa'] (advance pa)]
       )))
  (let [expr (comparison pa)]
    ))

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
