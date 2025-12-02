(ns advent-of-code-2025.day1
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn convert-dial-to-number
  [dial]
  (let [number (Integer/parseInt (subs dial 1))]
    (if (= (first dial) \R) number (- number))))

(defn secret-entrance
  []
  (let [input (->> (io/resource "inputs/day1.txt")
                   slurp
                   str/split-lines
                   (map convert-dial-to-number))]

    (loop [dial-pos 50
           dial-rotation (first input)
           dial-rest-rotations (rest input)
           password-count 0]
      (if (nil? dial-rotation)
        {:password password-count}
        (let [dial-rot-result (+ dial-pos dial-rotation)
              new-dial-pos (mod dial-rot-result 100)
              dial-num-rotations (quot (abs dial-rot-result) 100)
              new-password-count (if (pos? dial-rot-result)
                                   dial-num-rotations
                                   (if (zero? dial-pos)
                                     dial-num-rotations
                                     (+ dial-num-rotations 1)))]
          (recur new-dial-pos
                 (first dial-rest-rotations)
                 (rest dial-rest-rotations)
                 (+ password-count new-password-count)))))))
