(ns advent-of-code-2025.day2
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn range->str-seq
  [range-str]
  (let [rng-limits (->> (str/split range-str #"-")
                        (map Long/parseLong))]
    (map str (range (first rng-limits) (+ (second rng-limits) 1)))))

(defn get-id-divisors
  "Based on the number of chars on the id-str, get what numbers can divide the string evenly"
  [id-str]
  (let [total-digits (count id-str)]
    (filter
      #(= (mod total-digits %) 0)
      (range 1 total-digits))))

(defn get-seq-frequencies
  "Get the frequency of the sequences that each division gets when splitting the id-str"
  [id-str divisors]
  (for [div divisors]
    (->> (partition div id-str)
         frequencies
         (map #(count %)))))

(defn dedupe-frequencies
  "Remove repeated frequencies and remove the frequencies that are not equals to one, when a frequency is more than one
  mean that there were multiple not matching sequences in the id"
  [frq]
  (->> (distinct frq)
       (filter #(= (count %) 1))))

(defn invalid-id?
  "From the remaining frequencies after dedupe, only the frequencies that are not empty are the bad ids"
  [frq]
  (if (empty? frq) false true))

(defn invalid-ids
  "First iteration works, but is kinda slow"
  []
  (let [ranges (-> (io/resource "inputs/day2.txt")
                   slurp
                   (str/split #","))
        input (flatten (map range->str-seq ranges))]
    (reduce (fn [sum id]
              (let [is-bad-id? (->> (get-id-divisors id)
                                    (get-seq-frequencies id)
                                    dedupe-frequencies
                                    invalid-id?)]
                ;(println id " - " divisors " - " total-frequencies " - " cleaned-frequencies " - " is-bad-id?)
                (if is-bad-id? (+ sum (Long/parseLong id)) sum)))
            0 input)))
