(ns advent-of-code-2025.day9
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn get-coordinates
  []
  (->> (io/resource "inputs/day9.txt")
       slurp
       str/split-lines
       (mapv #(mapv Long/parseLong (str/split % #",")))))

(defn calc-area
  [[x1 y1] [x2 y2]]
  (let [l1 (abs (inc (- x1 x2)))
        l2 (abs (inc (- y1 y2)))]
    (* l1 l2)))

(defn red-tiles
  []
  (let [coordinates (get-coordinates)]
    (apply max (for [c1 coordinates c2 coordinates]
                 (calc-area c1 c2)))))

(defn inside-polygon? [polygon [px py]]
  (let [n (count polygon)]
    (loop [i 0 inside false]
      (if (>= i n)
        inside
        (let [[x1 y1] (polygon i)
              [x2 y2] (polygon (mod (inc i) n))
              intersect (and (or (and (> y1 py) (<= y2 py))
                                 (and (> y2 py) (<= y1 py)))
                             (< px (+ x1 (* (/ (- py y1) (- y2 y1)) (- x2 x1)))))]
          (recur (inc i) (if intersect (not inside) inside)))))))

(defn cross-edges?
  [[[x1 y1] [x2 y2]] [min-x max-x] [min-y max-y]]
  (cond
    (= x1 x2) (and (< min-x x1 max-x)
                   (not (or (>= (min y1 y2) max-y) (<= (max y1 y2) min-y))))
    (= y1 y2) (and (< min-y y1 max-y)
                   (not (or (>= (min x1 x2) max-x) (<= (max x1 x2) min-x))))))

(defn is-green? [polygon [x1 y1] [x2 y2]]
  (let [min-x (min x1 x2) max-x (max x1 x2)
        min-y (min y1 y2) max-y (max y1 y2)
        n (count polygon)
        edges (map (fn [i] [(polygon i) (polygon (mod (inc i) n))]) (range n))]
    (and (inside-polygon? polygon [(/ (+ min-x max-x) 2) (/ (+ min-y max-y) 2)])
         (not
           (some
             #(cross-edges? % [min-x max-x] [min-y max-y])
             edges)))))

(defn red-green-tiles []
  (let [coords (get-coordinates)]
    (apply max
           (for [i (range (count coords))
                 j (range (inc i) (count coords))
                 :when (is-green? coords (coords i) (coords j))]
             (* (inc (abs (- (first (coords i)) (first (coords j)))))
                (inc (abs (- (second (coords i)) (second (coords j))))))))))
