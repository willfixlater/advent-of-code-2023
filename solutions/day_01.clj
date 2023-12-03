(ns day-01
  (:require [clojure.java.io :as io]))

(defn part-one [& _args]
  (let [input (-> "day_01.txt"
                  io/resource
                  io/reader)]
    (reduce
     (fn [acc line]
       (let [digits (filter #(< 47 (int %) 58) line)
             calib-val (parse-long (str (first digits) (last digits)))]
         (println "Line: " line "\nCalib: " calib-val)
         (+ acc calib-val)))
     0
     (line-seq input))))

(def lexing-tree
  {\1 \1
   \2 \2
   \3 \3
   \4 \4
   \5 \5
   \6 \6
   \7 \7
   \8 \8
   \9 \9
   \o {\n {\e \1}}
   \t {\w {\o \2}
       \h {\r {\e {\e \3}}}}
   \f {\o {\u {\r \4}}
       \i {\v {\e \5}}}
   \s {\i {\x \6}
       \e {\v {\e {\n \7}}}}
   \e {\i {\g {\h {\t \8}}}}
   \n {\i {\n {\e \9}}}})

(defn lex-with [ctx char-seq]
  (let [c (first char-seq)
        cs (rest char-seq)]
    (when-let [lexed (get ctx c)]
      (cond
        (char? lexed) lexed
        (map? lexed) (recur lexed cs)))))

(defn lex-line [line]
  (loop [tokens []
         line line]
    (if (empty? line)
      tokens
      (if-let [token (lex-with lexing-tree line)]
        (recur (conj tokens token) (rest line))
        (recur tokens (rest line))))))

(defn part-two [& _args]
  (let [input (-> "day_01.txt"
                  io/resource
                  io/reader)
        lines (line-seq input)]
    (->> lines
         (map lex-line)
         (map (juxt first last))
         (map #(apply str %))
         (map parse-long)
         (reduce +))))

(comment
  (part-one)
  (part-two)
  )