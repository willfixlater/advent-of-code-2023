(ns day-02
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Game Grammar (integer is defined as a clojure integer literal)
;; game -> "Game " game-id ": " grabs
;; grabs -> (grab "; ")* grab
;; grab -> (dice ", ")* dice
;; dice -> die-count " " die-colour
;; die-count -> integer
;; die-colour -> "blue" | "red" | "green"

;; Games spec:
;; {game-id [{die-colour die-count, ...}, ...], ...}

(def input-path "day_02.txt")

(defn parse-games [lines]
  (reduce
   (fn [games line]
     (let [game-id (->> line
                        (drop 5)
                        (take-while (complement #{\:}))
                        (apply str)
                        parse-long)
           no-game-id* (->> line
                            (drop-while (complement #{\:}))
                            (drop 2)
                            (apply str))
           grabs* (map (comp #(map (fn [s] (str/split s #" ")) %)
                             #(str/split % #", "))
                       (str/split no-game-id* #"; "))
           grabs (reduce
                  (fn [grabs grab]
                    (conj grabs
                          (reduce
                           (fn [dice [die-count die-colour]]
                             (assoc dice
                                    (keyword die-colour)
                                    (parse-long die-count)))
                           {} grab)))
                  [] grabs*)]
       (assoc games game-id grabs)))
   {}
   lines))

(defn part-one [& _args]
  (let [input (-> input-path io/resource io/reader)
        lines (line-seq input)
        games (parse-games lines)
        possible-games (into {}
                             (comp
                              (map (fn [[game-id grabs]]
                                     [game-id
                                      (reduce
                                       (fn [{:keys [blue red green]} grab]
                                         {:blue (max (:blue grab 0) blue)
                                          :red (max (:red grab 0) red)
                                          :green (max (:green grab 0) green)})
                                       {:blue 0, :red 0, :green 0}
                                       grabs)]))
                              (filter (fn [[_ {:keys [blue red green]}]]
                                        (and (or (not blue) (<= blue 14))
                                             (or (not red) (<= red 12))
                                             (or (not green) (<= green 13))))))
                             games)]
    (reduce + (map first possible-games))))

(defn part-two [& _args]
  (let [input (-> input-path io/resource io/reader)
        lines (line-seq input)
        games (parse-games lines)
        min-dice-per-game (into {}
                                (map (fn [[game-id grabs]]
                                       [game-id
                                        (reduce
                                         (fn [{:keys [blue red green]} grab]
                                           {:blue (max (:blue grab 0) blue)
                                            :red (max (:red grab 0) red)
                                            :green (max (:green grab 0) green)})
                                         {:blue 0, :red 0, :green 0}
                                         grabs)]))
                                games)]
    (->> min-dice-per-game
         (map (fn [[_ {:keys [blue red green]}]]
                (* red blue green)))
         (reduce +))))

(comment
  (part-one)
  (part-two)
  )