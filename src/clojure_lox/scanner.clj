(ns clojure-lox.scanner)

(def ^:const eof (char 0))

(def ^:const keywords
  (->> ["and" "class" "else" "false" "for" "fun" "if" "nil"
        "or" "print" "return" "super" "this" "true" "var"
        "while"]
       (map (fn [name] [name (keyword (str "token/" name))]))
       (into {})))

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
  (str token-type " '" lexeme "' " literal))

(defn char-at
  "returns the char at the given positon or zero"
  [content index]
  (nth content index eof))

(defn scanner [s]
  {:content (vec s)
   :current 0
   :line    1})

(defn is-done?
  "checks if the scanner has scanned the whole content or returned
  an error"
  [{:keys [tokens errors]}]
  (boolean (or (seq errors)
               (= :token/eof
                  (some-> tokens
                          last
                          :token-type)))))

(defn error
  "adds an error the scanner and returns the scanner"
  [sc line message]
  (update sc :errors
          (fnil conj [])
          (str "[line " line "] Error: " message)))

(defn advance
  "advances the scanner to a new character"
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

(defn consume-line-comment [{:keys [current content] :as sc}]
  (loop [index current]
    (let [c (char-at content index)]
      (if (or (= eof c) (= \newline c))
        (-> sc
            (assoc :current index))
        (recur (inc index))))))

(defn scan-string [{:keys [line current content] :as sc}]
  (loop [index   (inc current)
         line    line
         escaped false]
    (let [c (char-at content index)]
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

        (= eof c)
        (error sc line "unterminated string.")

        :else
        (recur (inc index) line escaped)))))

(defn scan-number
  "Numbers can be integer of floating point numbers. Numbers
  can't start with or end with a dot. So '123' and '0.123' are
  valid numbers."
  [{:keys [content current line] :as sc}]
  (loop [index      current
         ;; is the scanner scanning the fraction?
         fractional false]
    (let [c (char-at content index)]
      (cond
        (Character/isDigit c)
        (recur (inc index) fractional)

        (and (= \. c)
             (Character/isDigit (char-at content (inc index)))
             (not fractional))
        (recur (inc index) true)

        :else
        (-> sc
            (add-token (make-token :token/number
                                   (Double/parseDouble
                                    (apply str (subvec content
                                                       current
                                                       index)))
                                   line))
            (assoc :current index))))))

(defn scan-identifier
  [{:keys [content current line] :as sc}]
  (loop [index current]
    (let [c (char-at content index)]
      (if (alpha? c)
        (recur (inc index))
        (let [lexeme  (apply str (subvec content current index))
              tk-type (get keywords lexeme :token/identifier)]
          (-> sc
              (add-token (make-token tk-type
                                     lexeme
                                     line))
              (assoc :current index)))))))

(defn scan-token [{:keys [content current line] :as sc}]
  (let [c  (char-at content current)
        nc (char-at content (inc current))]
    (case c
      eof (add-token sc (make-token :token/eof line))
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
      \! (if (= \= nc)
           (-> sc
               (add-token (make-token :token/bang-equal line))
               (advance 2))
           (add-token-and-advance sc (make-token :token/bang line)))
      \= (if (= \= nc)
           (-> sc
               (add-token (make-token :token/equal-equal line))
               (advance 2))
           (add-token-and-advance sc (make-token :token/equal line)))
      \< (if (= \= nc)
           (-> sc
               (add-token (make-token :token/less-equal line))
               (advance 2))
           (add-token-and-advance sc (make-token :token/less line)))
      \> (if (= \= nc)
           (-> sc
               (add-token (make-token :token/greater-equal line))
               (advance 2))
           (add-token-and-advance sc (make-token :token/greater line)))
      \/ (if (= \/ nc)
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

(defn debug-step [sc]
  (let [sc'       (scan-token sc)
        new-token (> (count (:tokens sc')) (count (:tokens sc)))]
    (println "Scanned token from index" (:current sc) "to" (:current sc'))
    (when new-token
      (println "\tnew Token" (some-> sc'
                                     :tokens
                                     last)))
    sc'))

(defn scan-tokens
  ([sc]
   (scan-tokens sc false))
  ([{:keys [line errors] :as sc} debug]
   (if (is-done? sc)
     sc
     (recur (if debug
              (debug-step sc)
              (scan-token sc))
            debug))))

(comment
  ; the first token of 
  ; var language = "lox";
  {:token-type :token/var
   :lexeme "var"
   :literal nil
   :line 1}



  (-> (scanner "*()//hello")
      scan-tokens)
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
  (-> (scanner "\"hello\" 123.4567 var vary")
      (scan-tokens true)
      :tokens
      (as-> tks (map token->str tks)))
  
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
