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

;; Part 2 Solution Strategry
;; 1. Set total cards to count of scratch cards
;; 2. Finding winning cards and record the card-no & win count
;; 3. Transform card-no & win count into list of card-nos
;; 4. Count card-nos and add them to total cards 
;; 5. If no card-nos continue, otherwise map card-nos to cards and recur to (2).
;; 6. Return total winning cards

(defn winning-nos [scratchcard]
  (let [win-nos (-> scratchcard :win-nos set)
        my-nos (-> scratchcard :my-nos set)]
    (set/intersection win-nos my-nos)))

(defn winning-cards [cards]
  (map #(assoc % :winning-nos (winning-nos %))
       cards))

(defn winning-cards->card-nos [winning-cards]
  (mapcat (fn [{:keys [no winning-nos]}]
            (take (count winning-nos) (iterate inc (inc no))))
          winning-cards))

(defn card-nos->cards [scratchcards card-nos]
  (map (into {} (map (juxt :no identity)) scratchcards)
       card-nos))

(defn ->find-new-cards [scratchcards]
  (comp (partial card-nos->cards scratchcards)
        winning-cards->card-nos
        winning-cards))

(defn part-two [& _args]
  (let [input (-> input-path io/resource io/reader)
        lines (line-seq input)
        scratchcards (map parse-scratchcard lines)
        find-new-cards (->find-new-cards scratchcards)]
    (loop [total-cards (count scratchcards)
           cards scratchcards]
      (if (empty? cards)
        total-cards
        (let [new-cards (find-new-cards cards)]
          (recur (+ total-cards (count new-cards))
                 new-cards))))))

(comment
  (part-one)
  (part-two)
  )