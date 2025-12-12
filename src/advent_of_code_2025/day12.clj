(ns advent-of-code-2025.day12
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn get-present-and-regions
  []
  (->> (io/resource "inputs/day12.txt")
       slurp
       str/split-lines
       (partition-by #(= % ""))
       (remove #(= (first %) ""))))

(defn calculate-fits
  [[[width height] num-presents] presents]
  (let [region-area (* width height)
        ; assume that 3x3 fits and see how it goes from here... for some reason ths worked
        total-taken-space (apply + (map #(* % 3 3) num-presents))]
    (if (<= total-taken-space region-area) 1 0)))

(defn presents
  []
  (let [input (get-present-and-regions)
        presents (drop-last input)
        regions (map (fn [region-str]
                       (let [parts (str/split region-str #": ")
                             dimensions-str (first parts)
                             numbers-str (second parts)
                             [width height] (map #(Integer/parseInt %) (str/split dimensions-str #"x"))
                             numbers (map #(Integer/parseInt %) (str/split numbers-str #" "))]
                         (vec (cons [width height] [numbers]))))
                     (last input))
        ;_ (println regions)
        total-fits (map #(calculate-fits % presents) regions)]
    (apply + total-fits)))
