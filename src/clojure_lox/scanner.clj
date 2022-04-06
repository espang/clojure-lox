(ns clojure-lox.scanner)

(defn alpha? [c]
  (or (= \_ c)
      (Character/isLetter c)))

(defn make-token
  ([tt line]
   (make-token tt "" line))
  ([tt lexeme line]
   (make-token tt lexeme nil line))
  ([tt lexeme literal line]
   {:token-type tt
    :lexeme lexeme
    :literal literal
    :line line}))

(defn token->str [{:keys [token-type lexeme literal]}]
  (str token-type " " lexeme " " literal))

(defn scanner [s]
  {:content (vec s)
   :current 0
   :line    1})

(defn is-at-end [{:keys [content current]}]
  (>= current (count content)))

(defn error [scanner line message]
  (update scanner :errors
          (fnil conj [])
          (str "[line " line "] Error: " message)))

(defn advance 
  ([sc]
   (advance sc 1))
  ([sc n]
   (update sc :current (fnil + 0) n)))

(defn add-token [sc token]
  (update sc :tokens (fnil conj []) token))

(defn add-token-and-advance [sc token]
  (-> sc
      (add-token token)
      advance))

(defn peek [{:keys [current content]}]
  (let [idx (inc current)]
    (if (>= idx (count content))
      \0
      (nth content idx))))

(defn consume-line-comment [{:keys [current content line] :as sc}]
  (loop [index current]
    (let [c (nth content index \newline)]
      (if (= \newline c)
        (-> sc
            (assoc :current (inc index))
            (update :line (fnil inc 1)))
        (recur (inc index))))))

(defn scan-string [{:keys [line current content] :as sc}]
  (loop [index   (inc current)
         line    line
         escaped false]
    (if (>= index (count content))
      (error sc line "unterminated string.")
      (let [c (nth content index)]
        (cond
          (and (not escaped)
               (= \" c))
          (-> sc
              (add-token (make-token :token/string 
                                     (apply str (subvec content
                                                        (inc current)
                                                        index))
                                     line))
              (assoc :line line)
              (assoc :current (inc index)))

          (= \\ c)
          (recur (inc index) line (not escaped))
          
          (= \newline c)
          (recur (inc index) (inc line) escaped)

          :else
          (recur (inc index) line escaped))))))

(defn scan-number [{:keys [content current line] :as sc}]
  (loop [index      current
         ;; is the scanner scanning the fraction?
         fractional false]
    (if (>= index (count content))
      (-> sc
          (add-token (make-token :token/number
                                 (Double/parseDouble
                                  (apply str (subvec content
                                                     current)))
                                 line))
          (assoc :current index))
      (let [c (nth content index)]
        (cond
          (Character/isDigit c)
          (recur (inc index) fractional)

          (and (= \. c)
               (Character/isDigit (peek {:content content :current index}))
               (not fractional))
          (recur (inc index) true)
          
          :else
          (-> sc
              (add-token (make-token :token-number
                                     (Double/parseDouble
                                      (apply str (subvec content
                                                         current
                                                         index)))
                                     line))
              (assoc :current index)))))))

(defn scan-identifier [sc]
  sc)

(defn scan-token [{:keys [current content line] :as sc}]
  (let [c (nth content current)]
    (case c
      \( (add-token-and-advance sc (make-token :token/left-paren line))
      \) (add-token-and-advance sc (make-token :token/right-paren line))
      \{ (add-token-and-advance sc (make-token :token/left-brace line))
      \} (add-token-and-advance sc (make-token :token/right-brace line))
      \, (add-token-and-advance sc (make-token :token/comma line))
      \. (add-token-and-advance sc (make-token :token/dot line))
      \- (add-token-and-advance sc (make-token :token/minus line))
      \+ (add-token-and-advance sc (make-token :token/plus line))
      \; (add-token-and-advance sc (make-token :token/semicolon line))
      \* (add-token-and-advance sc (make-token :token/star line))
      \! (if (= \= (peek sc))
           (-> sc
               (add-token (make-token :token/bang-equal line))
               (advance 2))
           (add-token-and-advance sc (make-token :token/bang line)))
      \= (if (= \= (peek sc))
           (-> sc
               (add-token (make-token :token/equal-equal line))
               (advance 2))
           (add-token-and-advance sc (make-token :token/equal line)))
      \< (if (= \= (peek sc))
           (-> sc
               (add-token (make-token :token/less-equal line))
               (advance 2))
           (add-token-and-advance sc (make-token :token/less line)))
      \> (if (= \= (peek sc))
           (-> sc
               (add-token (make-token :token/greater-equal line))
               (advance 2))
           (add-token-and-advance sc (make-token :token/greater line)))
      \/ (if (= \/ (peek sc))
           ;; is comment
           (consume-line-comment sc)
           (add-token-and-advance sc (make-token :token/slash line)))
      \space (advance sc)
      \tab (advance sc)
      \return (advance sc)
      \newline (-> sc
                   (update :line (fnil inc 1))
                   advance)
      \" (scan-string sc)
      (cond
        (Character/isDigit c)
        (scan-number sc)

        (alpha? c)
        (scan-identifier sc)

        :else
        (error sc line (str "unexpected token '" c "'"))))))

(defn scan-tokens [{:keys [line errors] :as sc}]
  (if (is-at-end sc)
    (update sc :tokens (fnil conj [])
            {:token-type :token/eof
             :line line})
    (if (seq errors)
      sc
      (recur (scan-token sc)))))

(comment
  ; the first token of 
  ; var language = "lox";
  {:token-type :token/var
   :lexeme "var"
   :literal nil
   :line 1}

  (->> (scanner "*()//hello")
       scan-tokens
       :tokens
       (map token->str))
  (->> (scanner "*()//hello\n{}")
       scan-tokens
       :tokens
       (map token->str))
  (->> (scanner "*() \n {}")
       scan-tokens
       :tokens
       (map token->str))
  (->> (scanner ">=>!!=<<=")
       scan-tokens
       :tokens
       (map token->str))
  (->> (scanner "\"hello\"")
       scan-tokens
       :tokens
       (map token->str))
  

  (-> {:current 0 :content (vec "123")}
      scan-number
      :tokens
      first)
  (-> {:current 0 :content (vec "123.12")}
      scan-number
      :tokens
      first)
  (-> {:current 0 :content (vec "123.12.12")}
      scan-number
      :tokens
      first)

  ) 
