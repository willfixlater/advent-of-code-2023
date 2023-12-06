(ns day-03
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Input Grammar
;; schematic -> row+
;; row       -> (integer | "."+ | symbol)+ "\n"
;; integer   -> digit+
;; digit     -> "0" | "1" | "2" | "3" | "4"
;;            | "5" | "6" | "7" | "8" | "9"
;; symbol    -> "@" | "#" | "$" | "%" | "&"
;;            | "*" | "+" | "-" | "/" | "="

;; Schematic Spec
;; {:numbers [{:val number, :bb bounding-box}, ...]
;;  :symbols [{:val symbol, :coord coord}, ...]}
;; where bounding-box -> [coord, coord]
;;       coord        -> [integer, integer]

(def input-path (str (str/replace *ns* #"-" "_") ".txt"))

(def digits #{\0 \1 \2 \3 \4 \5 \6 \7 \8 \9})

(def symbols #{\@ \# \$ \% \& \* \+ \- \/ \=})

(defn parse-schematic [input]
  ;; why do i hear boss music?
  (let [lines (line-seq input)]
    (loop [row 0
           [l & ls] lines
           schematic {:numbers []
                      :symbols []}]
      (if (nil? l)
        schematic
        (recur (inc row) ls
          (loop [col 0
                 [c & cs] l
                 context []
                 schematic schematic]
            (if (nil? c)
              (cond
                (seq context)
                (update schematic :numbers conj
                        {:val (parse-long (apply str context))
                         :bb [[(- col (count context)), row]
                              [col, (inc row)]]})
                :else schematic)
              (recur (inc col) cs
                (if (digits c)
                  (conj context c)
                  [])
                (if (digits c)
                  schematic
                  (cond
                    (and (symbols c) (seq context))
                    {:numbers (conj (:numbers schematic)
                                    {:val (parse-long (apply str context))
                                     :bb [[(- col (count context)), row]
                                          [col, (inc row)]]})
                     :symbols (conj (:symbols schematic)
                                    {:val c, :coord [col, row]})}
                    (symbols c)
                    (update schematic :symbols conj
                            {:val c, :coord [col, row]})
                    
                    (seq context)
                    (update schematic :numbers conj
                            {:val (parse-long (apply str context))
                             :bb [[(- col (count context)), row]
                                  [col, (inc row)]]})
                    
                    :else schematic))))))))))

(defn in-bounding-box? [[[left top] [right bottom]] [x y]]
  (and (<= left x (dec right))
       (<= top y (dec bottom))))

(defn inc-bounding-box [[[left top] [right bottom]]]
  [[(max 0 (dec left)) (max 0 (dec top))]
   [(inc right) (inc bottom)]])

(defn adjacent-to-bounding-box? [bb coord]
  (in-bounding-box? (inc-bounding-box bb) coord))

(defn part-one [& _args]
  (let [input (-> input-path io/resource io/reader)
        schematic (parse-schematic input)
        symbol-coords (map :coord (:symbols schematic))]
    (->> (:numbers schematic)
         (filter #(some (partial adjacent-to-bounding-box? (:bb %))
                        symbol-coords))
         (map :val)
         (reduce +))))

(defn part-two [& _args]
  (let [input (-> input-path io/resource io/reader)
        schematic (parse-schematic input)
        *-coords (->> (:symbols schematic)
                      (filter (comp #{\*} :val))
                      (map :coord))
        *-adjacent (dissoc
                    (group-by (fn [{:keys [_ bb]}]
                                (some #(and (adjacent-to-bounding-box? bb %) %)
                                      *-coords))
                              (:numbers schematic))
                    nil)
        gear-ratios (->> *-adjacent
                         (filter (fn [[_ numbers]] (< 1 (count numbers))))
                         (map (fn [[_ numbers]] (map :val numbers))))]
    (->> gear-ratios
         (map #(apply * %))
         (reduce +))))

(comment
  (part-one)
  (part-two)
  )