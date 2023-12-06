(ns day-05
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Part 1 Input Grammar
;; almanac -> seeds-paragraph (paragraph-separator mapping-paragraph)*
;; seeds-paragraph -> "seeds: " seed (" " seed)*
;; mapping-paragraph -> source "-to-" destination " map:\n" map-ranges
;; map-ranges -> map-range ("\n" map-range)*
;; map-range -> destination-start " " source-start " " range
;; seed -> integer
;; destination-start -> integer
;; source-start -> integer
;; range -> integer
;; source -> string
;; destination -> string
;; paragraph-separator -> "\n\n"
;; where integer is a clojure integer literal
;; and string is a sequence of alpha characters

;; Part 1 Almanac Spec
;; {:seeds [seed, ...]
;;  :maps {(keyword source) {:destination (keyword destination)
;;                           :ranges [[destination-start
;;                                     source-start
;;                                     range], ...]}, ...}}

;; Part 1 Solution Strategry
;; 1. Given a function that recursively maps from source to destination,
;;    map each seed to its final destination.
;; 2. Find the minimum of the results

(def input-path (str (str/replace *ns* #"-" "_") ".txt"))

(defn parse-almanac [input]
  (let [seed "(\\d+)"
        seeds-paragraph (str "seeds:\\s*" (str/join "\\s+" (repeat 20 seed)))
        paragraph-separator "\n\n"
        map-range "\\d+\\s+\\d+\\s+\\d+"
        map-ranges (str map-range "(?:\n" map-range ")*" )
        mapping-paragraph-header "(\\w+)-to-(\\w+)\\s+map:"
        mapping-paragraph (str mapping-paragraph-header "\\s+"
                               "(" map-ranges ")")
        mapping-paragraphs (str/join paragraph-separator
                                     (repeat 7 mapping-paragraph))
        regex (re-pattern (str "^" seeds-paragraph
                               paragraph-separator mapping-paragraphs))
        regex-result (re-find regex (slurp input))]
    {:seeds (mapv parse-long (subvec regex-result 1 21))
     :ranges (->> regex-result
                  (drop 21)
                  (partition 3)
                  (map (fn [[src dest rng]]
                         [(keyword src)
                          {:destination (keyword dest)
                           :ranges (->> (str/split-lines rng)
                                        (map #(str/split % #"\s+"))
                                        (mapv (fn [[dr sr r]]
                                                [(parse-long dr)
                                                 (parse-long sr)
                                                 (parse-long r)])))}]))
                  (into {}))}))

(defn ->resolve-seed [map-ranges]
  (fn [seed]
    (loop [input seed
           {:keys [destination ranges]} (:seed map-ranges)]
      (let [output (loop [[[dest src delta] & remaining] ranges]
                     (cond
                       (<= src input (+ src (dec delta)))
                       (+ dest (- input src))

                       (empty? remaining)
                       input

                       :else
                       (recur remaining)))]
        (if-let [map-range (map-ranges destination)]
          (recur output map-range)
          output)))))

(defn part-one [& _args]
  (let [input (-> input-path io/resource io/reader)
        almanac (parse-almanac input)
        resolve-seed (->resolve-seed (:ranges almanac))]
    (apply min (map resolve-seed (:seeds almanac)))))


(comment
  (part-one)
  (part-two)
  )