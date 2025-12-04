(ns advent-of-code-2025.day4
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn get-diagram
  []
  (->> (io/resource "inputs/day4.txt")
       slurp
       str/split-lines
       (map vec)))

(defn get-adjacent
  [r c]
  (mapv keyword
        [(str (- r 1) "-" (- c 1))
         (str (- r 1) "-" c)
         (str (- r 1) "-" (+ c 1))
         (str r "-" (- c 1))
         (str r "-" (+ c 1))
         (str (+ r 1) "-" (- c 1))
         (str (+ r 1) "-" c)
         (str (+ r 1) "-" (+ c 1))]))

(defn rolls-of-paper
  []
  (let [diagram (get-diagram)
        rolls (second
                (reduce
                  (fn [[rn m] row]
                    (let [new-m (reduce-kv
                                  (fn [m idx val]
                                    (let [k (keyword (str rn "-" (inc idx)))]
                                      (if (= val \@)
                                        (assoc m k (get-adjacent rn (inc idx)))
                                        m)))
                                  {} row)]
                      [(inc rn) (merge m new-m)]))
                  [1 {}] diagram))
        rolls-and-adjacent (map #(keys (select-keys rolls %)) (vals rolls))
        available-rolls (filter #(< (count %) 4) rolls-and-adjacent)]
    (count available-rolls)))

(defn rolls-of-paper-2
  []
  (let [diagram (get-diagram)
        rolls (second
                (reduce
                  (fn [[rn m] row]
                    (let [new-m (reduce-kv
                                  (fn [m idx val]
                                    (let [k (keyword (str rn "-" (inc idx)))]
                                      (if (= val \@)
                                        (assoc m k (get-adjacent rn (inc idx)))
                                        m)))
                                  {} row)]
                      [(inc rn) (merge m new-m)]))
                  [1 {}] diagram))]
    (loop [current-rolls rolls
           retired-rolls true
           retired-count-rolls 0]
      ;(println (keys current-rolls) " - " retired-count-rolls " - " retired-rolls)
      (if (not retired-rolls)
        retired-count-rolls
        (let [rolls-and-adjacent (into {} (map (fn [[k v]] {k (select-keys current-rolls v)}) current-rolls))
              rolls-to-retire (filter (fn [[k v]] (< (count v) 4)) rolls-and-adjacent)]
          (recur
            (reduce dissoc current-rolls (keys rolls-to-retire))
            (if (zero? (count rolls-to-retire)) false true)
            (+ retired-count-rolls (count rolls-to-retire))))))))
