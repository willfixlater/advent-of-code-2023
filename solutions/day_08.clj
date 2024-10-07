(ns day-08
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Input Grammar
;; documents -> steps "\n\n" map-graph
;; steps -> ("R" | "L")+
;; map-graph -> map-node ("\n" map-node)*
;; map-node -> node-name " = (" node-name ", " node-name ")"
;; node-name -> string
;; where string is a clojure string literal

;; Sheet Spec
;; {:steps (#[fn second] #[fn first] #[fn first], ...)
;;  :map-graph {node-name [node-name node-name], ...}}

;; Part 1 Solution Strategry
;;

;; Credit: https://github.com/clojure/math.numeric-tower/blob/a3cd25fa1427fa7deda943274e6c869cbd01c276/src/main/clojure/clojure/math/numeric_tower.clj#L192
(defn gcd
  "(gcd a b) returns the greatest common divisor of a and b"
  [a b]
  (loop [a (abs a)
         b (abs b)]
    (if (zero? b) a,
        (recur b (mod a b)))))

;; Credit: https://github.com/clojure/math.numeric-tower/blob/a3cd25fa1427fa7deda943274e6c869cbd01c276/src/main/clojure/clojure/math/numeric_tower.clj#L199
(defn lcm
  "(lcm a b) returns the least common multiple of a and b"
  [a b]
  (cond (zero? a) 0
        (zero? b) 0
        :else (abs (* b (quot a (gcd a b))))))

(def input-path (str (str/replace *ns* #"-" "_") ".txt"))

(defn parse-map-node [node-str]
  (let [[value branches] (str/split node-str #" = ")]
    [value [(subs branches 1 4) (subs branches 6 9)]]))

(defn parse-documents [input]
  (let [lines (line-seq input)]
    {:steps (cycle (map {\L first, \R second} (first lines)))
     :map-graph (reduce (fn [graph node-str]
                          (conj graph (parse-map-node node-str)))
                        {}
                        (drop 2 lines))}))

(defn part-one [& _args]
  (let [input (-> input-path io/resource io/reader)
        {:keys [steps map-graph]} (parse-documents input)]
    (loop [cnt 1
           node "AAA"
           [step & remaining] steps]
      (let [new-node (step (map-graph node))]
        (if (= new-node "ZZZ")
          cnt
          (recur (inc cnt) new-node remaining))))))

(defn part-two [& _args]
  (let [input (-> input-path io/resource io/reader)
        {:keys [steps map-graph]} (parse-documents input)
        all-node-names (keys map-graph)]
    (loop [cnt 1
           nodes (->> all-node-names
                      (filter #(str/ends-with? % "A"))
                      (mapv (juxt (constantly nil) identity)))
           [step & remaining] steps]
      (let [new-nodes (mapv (fn [[found node]]
                              (let [new-node (step (map-graph node))]
                                (cond
                                  (str/ends-with? node "Z")
                                  [found node]
                                  (str/ends-with? new-node "Z")
                                  [cnt new-node]
                                  :else
                                  [found new-node])))
                            nodes)]
        (if (->> new-nodes
                 (map second)
                 (every? #(str/ends-with? % "Z")))
          (->> new-nodes (map first) (reduce lcm))
          (recur (inc cnt) new-nodes remaining))))))

(comment
  (part-one)
  (part-two)
  )