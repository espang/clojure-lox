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

(defn error-msg
  "adds an error the scanner and returns the scanner"
  [line message]
  (str "[line " line "] Error: " message))

(defn add-error [sc error-msg]
  (update sc :errors (fnil conj []) error-msg))

(defn add-token [sc token]
  (update sc :tokens (fnil conj []) token))

(defn scan-line-comment [from content]
  (loop [index from]
    (let [c (char-at content index)]
      (if (or (= c eof)
              (= c \newline))
        [index (apply str (subvec content from index))]
        (recur (inc index))))))

(defn scan-string [from content]
  (loop [index   (inc from)
         line    0
         escaped false]
    (let [c (char-at content index)]
      (cond
        (and (not escaped)
             (= \" c))
        [:ok
         (inc index)
         (apply str (subvec content
                            (inc from)
                            index))
         line]
        
        (= \\ c)
        (recur (inc index) line (not escaped))

        (= \newline c)
        (recur (inc index) (inc line) escaped)

        (= eof c)
        [:error/unterminated-string]

        :else
        (recur (inc index) line escaped)))))

(defn scan-number
  "Numbers can be integer of floating point numbers. Numbers
  can't start with or end with a dot. So '123' and '0.123' are
  valid numbers."
  [from content]
  (loop [index from
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
        [index
         (Double/parseDouble
          (apply str (subvec content from index)))]))))

(defn scan-identifier
  [from content]
  (loop [index from]
    (let [c (char-at content index)]
      (if (alpha? c)
        (recur (inc index))
        [index
         (apply str (subvec content from index))]))))

(defn scan-next-token
  "returns the next token as a triple of the index after the token,
  the token and optionally the lexeme and the lines spanned. Returns
  nil and an error-code for errors."
  [from content]
  (let [c      (char-at content from)
        nc     (char-at content (inc from))
        result (case c
                 eof {:token :token/eof}
                 \(  {:token :token/left-paren}
                 \)  {:token :token/right-paren}
                 \{  {:token :token/left-brace}
                 \}  {:token :token/right-brace}
                 \,  {:token :token/comma}
                 \.  {:token :token/dot}
                 \-  {:token :token/minus}
                 \+  {:token :token/plus}
                 \;  {:token :token/semicolon}
                 \*  {:token :token/star}
                 \!  (if (= \= nc)
                       {:token :token/bank-equal
                        :after (+ from 2)}
                       {:token :token/bang})
                 \= (if (= \= nc)
                      {:token :token/equal-equal
                       :after (+ from 2)}
                      {:token :token/equal})
                 \< (if (= \= nc)
                      {:token :token/less-equal
                       :after (+ from 2)}
                      {:token :token/less})
                 \> (if (= \= nc)
                      {:token :token/greater-equal
                       :after (+ from 2)}
                      {:token :token/greater})
                 \/ (if (= \/ nc)
                      (let [[after _] (scan-line-comment from content)]
                        {:token :ignore
                         :after after})
                      {:token :token/slash})
                 \space {:token :ignore}
                 \tab {:token :ignore}
                 \return {:token :ignore}
                 \newline {:token :ignore
                           :span-lines 1}
                 \" (let [[error-code after lexeme span-lines] (scan-string from content)]
                      (case error-code
                        :ok {:token :token/string
                             :value lexeme
                             :span-lines span-lines
                             :after after}
                        :else {:error error-code}))
                 (cond
                   (Character/isDigit c)
                   (let [[after number] (scan-number from content)]
                     {:token :token/number
                      :value number
                      :after after})
                   
                   (alpha? c)
                   (let [[after value] (scan-identifier from content)
                         token-type    (get keywords value :token/identifier)]
                     {:token token-type
                      :value value
                      :after after})
                   
                   (= eof c)
                   {:token :token/eof}

                   :else
                   {:error :error/unexpected-token
                    :value c}))]
    (merge {:after (inc from)
            :span-lines 0}
           result)))

(defn scan-token [{:keys [content current line] :as sc}]
  (let [{:keys [after error span-lines token value]} (scan-next-token current content)]
    (if (nil? error)
      (cond-> (-> sc
                  (assoc :current after)
                  (update :line + span-lines))
        (not= :ignore token)
        (add-token (make-token token value line)))
      (add-error sc 
                 (case error
                   :error/unterminated-string (error-msg line "unterminated string." )
                   :error/unexpected-token    (error-msg line (str "unexpected character '" value "'")))))))

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
  (-> (scanner "*()//hello")
      scan-tokens)
  (-> (scanner "\"hello\" 123.4567 var vary")
      (scan-tokens true)
      :tokens
      (as-> tks (map token->str tks)))) 
