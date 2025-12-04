(ns advent-of-code-2025.day3
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn get-banks
  []
  (-> (io/resource "inputs/day3.txt")
      slurp
      str/split-lines))

(defn get-max-joltage
  [bank]
  (reduce-kv (fn [[max sec-max] idx num]
               ;(println max sec-max " - " idx " - " num)
               (let [new-max (if (and (> num max)
                                      (not= idx (dec (count bank)))) num max)
                     new-sec-max (if (not= new-max max)
                                   0
                                   (if (> num sec-max) num sec-max))]
                 [new-max new-sec-max]))
             [0 0] bank))

(defn battery-banks-1
  []
  (let [banks (get-banks)
        totals (for [bank banks]
                 (let [bank-in-numbers (mapv #(Integer/parseInt (str %)) bank)]
                   (Integer/parseInt
                     (apply str (get-max-joltage bank-in-numbers)))))]

    (apply + totals)))

(defn get-sub-banks-coll
  "Get a coll of all the sub banks inside bank, by removing the first digit on each iteration"
  [bank]
  (for [i (range (count bank))]
    (apply str (drop i bank))))

(defn filter-sub-banks-by-length
  "Filters sub banks strings by length"
  [banks length]
  (filter #(>= (count (str %)) length) banks))

(defn sort-sub-banks
  "Sort sub banks by the first digit of the bank string and by their length
  So 8181 81 818181 11100 100000 will be sorted to 818181 8181 81 100000 11100
  "
  [banks]
  (reverse
    (sort-by
      (fn [s] [(read-string (str (first s))) (count s)])
      banks)))

(defn battery-banks-2
  []
  (let [banks (get-banks)
        totals (for [bank banks]
                 (loop [current-bank bank
                        jolteages []
                        spaces-left 12]
                   ;(println jolteages " - " spaces-left " - " current-bank)
                   (if (zero? spaces-left)
                     (Long/parseLong (apply str jolteages))
                     (let [sub-banks (-> (get-sub-banks-coll current-bank)
                                  (filter-sub-banks-by-length spaces-left)
                                  sort-sub-banks)
                           selected-bank (first sub-banks)]
                       (recur
                         (subs selected-bank 1)             ;Remove the first digit since it will be added to the jolteages
                         (conj jolteages (first selected-bank)) ; Lock the number to the jolteages
                         (dec spaces-left))))))]
    (apply + totals)))
