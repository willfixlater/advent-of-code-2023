(ns day-06
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

;; Input Grammar
;; sheet -> times "\n" distances
;; times -> "Time:" " "+  time (" "+ time)+
;; time -> integer
;; distances -> "Distance:" " "+  distance (" "+ distance)+
;; distance -> integer
;; where integer is a clojure integer literal

;; Sheet Spec
;; [{:time time, :record distance}, ...]

;; Part 1 Solution Strategry
;; 

(def input-path (str (str/replace *ns* #"-" "_") ".txt"))

(defn parse-line [prefix-match one-race?]
  (comp (if one-race?
          #(list (edn/read-string (str/replace % #"\s+" "")))
          #(map edn/read-string (str/split % #"\s+")))
        str/trim
        #(str/replace % prefix-match "")))

(defn ->race-times [one-race?]
  (parse-line #"Time:" one-race?))

(defn ->distance-records [one-race?]
  (parse-line #"Distance:" one-race?))

(defn parse-sheet [input one-race?]
  (let [[race-times distance-records]
        (map #((%1 one-race?) %2)
             [->race-times ->distance-records]
             (line-seq input))]
    (map (fn [time record]
           {:time time, :record record})
         race-times
         distance-records)))

(defn count-winning-moves [{:keys [time record]}]
  (let [min-hold (loop [hold 1]
                   (if (> (* hold (- time hold)) record)
                     hold
                     (recur (inc hold))))]
    (- (inc time) (* 2 min-hold))))

(defn part-one [& _args]
  (let [input (-> input-path io/resource io/reader)
        sheet (parse-sheet input false)]
    (apply * (map count-winning-moves sheet))))

(defn part-two [& _args]
  (let [input (-> input-path io/resource io/reader)
        sheet (parse-sheet input true)]
    (count-winning-moves sheet)))

(comment
  (part-one)
  (part-two)
  )