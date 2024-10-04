(ns day-XX
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(def input-path (str (str/replace *ns* #"-" "_") ".txt"))

(defn part-one [& _args]
  (let [input (-> input-path io/resource io/reader)]))

(defn part-two [& _args]
  (let [input (-> input-path io/resource io/reader)]))

(comment
  (part-one)
  (part-two))