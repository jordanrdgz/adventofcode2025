(ns advent-of-code-2025.day5
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn range-str->long-vec
  [range-str]
  (let [rng-limits (->> (str/split range-str #"-")
                        (map Long/parseLong))]
    [(first rng-limits) (second rng-limits)]))

(defn get-database
  []
  (->> (io/resource "inputs/day5.txt")
       slurp
       str/split-lines))

(defn fresh-ingredients
  []
  (let [database (get-database)
        [ranges-str _ ids-str] (partition-by #(= "" %) database)
        ranges (map range-str->long-vec ranges-str)
        ids (mapv Long/parseLong ids-str)
        fresh-ids (-> (for [[x y] ranges]
                        (filterv #(and (>= % x) (<= % y)) ids))
                      flatten
                      distinct)]
    (count fresh-ids)))

(defn fresh-ingredients-2
  []
  (let [database (get-database)
        [ranges-str _ _] (partition-by #(= "" %) database)
        ranges (sort-by first (map range-str->long-vec ranges-str))
        merged-ranges (reduce
                        (fn [new-ranges range]
                          (let [range-start (first range)
                                range-end (last range)]
                            (if (empty? new-ranges)
                              (conj new-ranges [range-start range-end])
                              (let [new-last (last new-ranges)
                                    new-start (first new-last)
                                    new-end (last new-last)]
                                (if (<= range-start (+ 1 new-end))
                                  (conj (pop new-ranges) [new-start (max new-end range-end)])
                                  (conj new-ranges range))))))
                        []
                        ranges)

        fresh-ids (for [[x y] merged-ranges]
                    (+ 1 (- y x)))]
    (apply + fresh-ids)))
