(ns advent-of-code-2025.day10
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))


(defn clean-input
  [manual]
  (map (fn [x] (map #(-> % vec rest drop-last) x)) manual))

(defn get-manual
  []
  (->> (io/resource "inputs/day10.txt")
       slurp
       str/split-lines
       (map #(str/split % #"\s"))))

(defn convert-binary
  [v]
  (map (fn [x] (if (= x "#") 1 0)) v))

(defn convert-buttons-to-binary
  [diagram-width v]
  (reduce #(assoc %1 %2 1)
          (vec (repeat diagram-width 0))
          v))

(defn factory-lights
  []
  (let [manual (get-manual)
        result (for [indicators manual]
                 (let [diagram (->> (first indicators)
                                    vec
                                    (map str)
                                    rest
                                    drop-last
                                    convert-binary)
                       diagram-width (count diagram)
                       buttons (->> indicators
                                    rest
                                    drop-last
                                    (map read-string)
                                    (map #(convert-buttons-to-binary diagram-width %)))
                       joltage (->> (last indicators)
                                    (re-seq #"\d+")
                                    (map #(Integer/parseInt %)))]

                   ;(println [diagram buttons joltage])
                   (loop [results buttons
                          presses 1]
                     (println presses " - " results)
                     (if (some #(= % diagram) results)
                       presses
                       (let [new-results (for [a buttons
                                               b results]
                                           (map bit-xor a b))]
                         ;(Thread/sleep 2000)
                         (recur (distinct new-results) (inc presses)))))))]
    (apply + result)))

; Not working... :D :D
(defn factory-lights-2
  []
  (let [manual (get-manual)
        result (for [indicators manual]
                 (let [diagram (->> (first indicators)
                                    vec
                                    (map str)
                                    rest
                                    drop-last
                                    convert-binary)
                       diagram-width (count diagram)
                       buttons (->> indicators
                                    rest
                                    drop-last
                                    (map read-string)
                                    (map #(convert-buttons-to-binary diagram-width %)))
                       joltage (->> (last indicators)
                                    (re-seq #"\d+")
                                    (map #(Integer/parseInt %)))

                       ;joltage-indexed (sort-by second < (map-indexed vector joltage))

                       decomposed-joltage (loop [start joltage
                                                 new-joltage []]
                                            ;(println start " - " new-joltage)
                                            ;(Thread/sleep 2000)
                                            (if (every? zero? start)
                                              (drop-last new-joltage)
                                              (let [j (map #(if (zero? %) 0 (dec %)) start)
                                                    i (map #(if (zero? %) 0 1) start)]
                                                (recur j (conj new-joltage i)))))
                       ]


                   (println [diagram buttons joltage decomposed-joltage])

                   (apply + (for [joltage decomposed-joltage]
                              (loop [results buttons
                                     presses 1]
                                ;(println presses " - " current-limits " - " results)
                                ;(println presses " - " (count results) " - " joltage-limit " - " results)
                                ;(println presses " - " joltage " - " results)
                                (if (some #(= % joltage) results)
                                  presses
                                  (let [
                                        new-results (for [a buttons
                                                          b results]
                                                      (map + a b))]
                                    ;(Thread/sleep 2000)
                                    (recur
                                      (distinct new-results)
                                      (inc presses)))))))
                   ))

        ]
    ;(apply + result)
    result))
