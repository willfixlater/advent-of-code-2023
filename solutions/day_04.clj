(ns day-04
  (:require [clojure.java.io :as io]
            [clojure.math :as math]
            [clojure.set :as set]
            [clojure.string :as str]))

;; Input Grammar
;; scratchcards -> (scratchcard "\n")+
;; scratchcard -> "Card " card-no
;;                ": " win-no (" " win-no)+
;;                " | " my-no (" " my-no)+
;; card-no, win-no, my-no -> integer
;; where integer is a clojure integer literal

;; Scratchards Spec
;; [{:no card-no
;;   :win-nos [win-no, ...]
;;   :my-nos [my-no, ...]}, ...]

;; Part 1 Solution Strategry
;; 1. For each card:
;;    a) Take the set intersection of win-nos and my-nos
;;    b) Count the result, decrement and then raise 2 to that number
;; 3. Sum the result 

(def input-path (str (str/replace *ns* #"-" "_") ".txt"))

(defn parse-scratchcard [line]
  (let [card-preface "Card\\s*(\\d+)"
        win-nos (str/join "\\s*" (repeat 10 "(\\d+)"))
        my-nos (str/join "\\s*" (repeat 25 "(\\d+)"))
        regex (re-pattern (str "^" card-preface
                               "\\s*:\\s*" win-nos
                               "\\s*\\|\\s*" my-nos
                               "\\s*$"))
        matches (re-matches regex line)]
    {:no (parse-long (second matches))
     :win-nos (mapv parse-long (subvec matches 2 12))
     :my-nos (mapv parse-long (subvec matches 12 37))}))

(defn part-one [& _args]
  (let [input (-> input-path io/resource io/reader)
        lines (line-seq input)
        scratchcards (map parse-scratchcard lines)]
    (->> scratchcards
         (map (juxt (comp set :win-nos)
                    (comp set :my-nos)))
         (map #(apply set/intersection %))
         (map (comp int #(math/pow 2 %) dec count))
         (reduce +))))

(defn part-two [& _args]
  (let [input (-> input-path io/resource io/reader)]
    ))

(comment
  (part-one)
  (part-two)
  )