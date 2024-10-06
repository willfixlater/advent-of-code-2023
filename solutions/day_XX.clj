(ns day-XX
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

;; Input Grammar
;; example -> something " " something-else
;; something-else -> something " else"
;; something -> string
;; where string is a clojure string literal

;; Sheet Spec
;; {:example [something something-else]}

;; Part 1 Solution Strategry
;; 1. Example strategy step
;; 2. Something in the second step

(def input-path (str (str/replace *ns* #"-" "_") ".txt"))

(defn part-one [& _args]
  (let [input (-> input-path io/resource io/reader)]))

(defn part-two [& _args]
  (let [input (-> input-path io/resource io/reader)]))

(comment
  (part-one)
  (part-two))