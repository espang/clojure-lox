(ns clojure-lox.core
  (:require [clojure-lox.scanner :as scanner]))

(defn execute [s]
  (let [sc     (scanner/scanner s)
        {:keys [tokens errors]} (scanner/scan-tokens sc)]
    (if (seq errors)
      (do
        (println "run with errors:" errors)
        65)
      (do
        (println "run successfully")
        (->> tokens
             (map scanner/token->str)
             (map println)
             doall)
        0))))

(defn prompt! [] (print "> ") (flush))

(defn lox-repl []
  (prompt!)
  (loop [line (read-line)]
    (if line
      (do
        (execute line)
        (prompt!)
        (recur (read-line)))
      (println "clojure-lox is done. Cancelled by Ctrl+D"))))

(defn execute-file [f]
  (execute (slurp f)))

(defn- run-args [args]
  (case (count args)
    0 (do
        (lox-repl)
        0)
    1 (execute-file (first args))
    (do
      (println "usage: clojure-lox [script-name]")
      42)))

(defn run [opts]
  (let [args *command-line-args*]
    (println "running clojure-lox" (first args) " with args:" (rest args))
    (let [exit-code (run-args (rest args))]
      (println "clojure-lox done:" exit-code)
      (System/exit exit-code))))

(comment
  (run-args ["f3.lox"]))
