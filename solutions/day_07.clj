(ns day-07
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Input Grammar
;; hand-list -> hand-and-bid ("\n" hand-and-bid)*
;; hand-and-bid -> hand " " bid
;; hand -> card card card card card
;; card -> "2" | "3" | "4" | "5" | "6" | "7" | "8"
;;       | "9" | "T" | "J" | "Q" | "K" | "A"
;; bid -> integer
;; where integer is a clojure integer literal

;; Sheet Spec
;; [{:hand hand, :bid bid, :type hand-type}, ...]

;; Part 1 Solution Strategry
;; 

(def ^:dynamic *J-is-joker?* false)

(defn card-rank-asc []
  (if *J-is-joker?*
    [\J \2 \3 \4 \5 \6 \7 \8 \9 \T \Q \K \A]
    [\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A]))

(defn hand-rank-asc []
  [:high-card
   :pair
   :two-pair
   :three-of-a-kind
   :full-house
   :four-of-a-kind
   :five-of-a-kind])

(def input-path (str (str/replace *ns* #"-" "_") ".txt"))

(defn ->hand-type [hand]
  (let [joker-count (if *J-is-joker?*
                      (count (keep #{\J} hand))
                      0)
        no-jokers (if *J-is-joker?*
                    (remove #{\J} hand)
                    hand)
        card-freqs (frequencies no-jokers)
        freq-freqs (frequencies (vals card-freqs))
        highest-freq (if (seq card-freqs)
                       (apply max (map val card-freqs))
                       0)]
    (cond
      (= 5 (+ highest-freq joker-count))
      :five-of-a-kind
      (= 4 (+ highest-freq joker-count))
      :four-of-a-kind
      (or (and (freq-freqs 3) (freq-freqs 2))
          (and (> joker-count 0) (= 2 (freq-freqs 2))))
      :full-house
      (= 3 (+ highest-freq joker-count))
      :three-of-a-kind
      (or (= 2 (freq-freqs 2))
          (and (> joker-count 0) (freq-freqs 2)))
      :two-pair
      (= 2 (+ highest-freq joker-count))
      :pair
      (and (= 1 highest-freq) (= 0 joker-count))
      :high-card
  
      :else
      (throw (ex-info "unknown hand type" {:hand hand})))))

(comment
  (binding [*J-is-joker?* true]
    (->hand-type "QAQJA"))
  )

(defn parse-hand-list [input]
  (->> (line-seq input)
       (map str/trim)
       (map #(str/split % #"\s+"))
       (map (fn [[hand-str bid-str]]
              (let [hand (vec hand-str)]
                {:hand hand
                 :bid (parse-long bid-str)
                 :type (->hand-type hand)})))))

(def hand-type->hand-rank
  (zipmap (hand-rank-asc) (range)))

(defn hand->cards-rank [hand]
  (let [card->rank (zipmap (card-rank-asc) (range))]
    (->> hand
         (map card->rank)
         (map * [100000000 1000000 10000 100 1])
         (reduce +))))

(defn determine-winnings [hand-list]
  (->> hand-list
       (sort-by (juxt (comp hand-type->hand-rank :type)
                      (comp hand->cards-rank :hand)))
       (map-indexed (fn [idx {:keys [bid]}] (* bid (inc idx))))
       (reduce +)))

(defn part-one [& _args]
  (let [input (-> input-path io/resource io/reader)
        hand-list (parse-hand-list input)]
    (determine-winnings hand-list)))

(defn part-two [& _args]
  (binding [*J-is-joker?* true]
    (let [input (-> input-path io/resource io/reader)
          hand-list (parse-hand-list input)]
      (determine-winnings hand-list))))

(comment
  (part-one)
  (part-two)
  )