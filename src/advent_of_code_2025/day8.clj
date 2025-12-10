(ns advent-of-code-2025.day8
  (:require [clojure.java.io :as io]
            [clojure.math :as math]
            [clojure.string :as str]))

(defn get-coordinates
  []
  (->> (io/resource "inputs/day8.txt")
       slurp
       str/split-lines
       ;(map #(str/split % #","))
       (mapv #(mapv Integer/parseInt (str/split % #","))))) ;Can this be like map -> comp -> partial twice?

(defn calc-distance
  [[x1 y1 z1] [x2 y2 z2]]
  (math/sqrt
    (+
      (math/pow (- x1 x2) 2)
      (math/pow (- y1 y2) 2)
      (math/pow (- z1 z2) 2))))

(defn circuits
  []
  (let [coordinates (get-coordinates)
        the-keys (map #(-> % str keyword) (range 1 (inc (count coordinates))))
        coordinates-map (zipmap the-keys coordinates)
        distances (into {}
                        (for [[k1 v1] coordinates-map
                              [k2 v2] coordinates-map]
                          (when (and (not= k1 k2) (< (Integer/parseInt (name k1)) (Integer/parseInt (name k2))))
                            (let [new-key (keyword (str (name k1) "-" (name k2)))
                                  new-value (calc-distance v1 v2)]
                              [new-key new-value]))))
        distances-sorted (sort-by second distances)
        ;_ (println distances-sorted)
        needed-circuits (->> (take 10 distances-sorted)
                             keys
                             (mapv #(str/split (name %) #"-")))
        ;_ (println (count needed-circuits))
        circuits (reduce
                   (fn [groups circuit]
                     (let [connected (filter #(seq (clojure.set/intersection (set %) (set circuit))) groups)
                           not-connected (remove (set connected) groups)]
                       (if (empty? connected)
                         (conj groups circuit)
                         (conj not-connected (vec (distinct (flatten (conj connected circuit))))))))
                   [] needed-circuits)]
    (->> circuits
         (sort-by count >)
         (take 3)
         ;(map #(map Integer/parseInt %))
         (map count)
         (apply *))))

(def take-circuits 4710)

(defn circuits-2
  []
  (let [coordinates (get-coordinates)
        total-coordinates (count coordinates)
        the-keys (map #(-> % str keyword) (range 1 (inc total-coordinates)))
        coordinates-map (zipmap the-keys coordinates)
        distances (into {}
                        (for [[k1 v1] coordinates-map
                              [k2 v2] coordinates-map]
                          (when (and (not= k1 k2) (< (Integer/parseInt (name k1)) (Integer/parseInt (name k2))))
                            (let [new-key (keyword (str (name k1) "-" (name k2)))
                                  new-value (calc-distance v1 v2)]
                              [new-key new-value]))))
        distances-sorted (sort-by second distances)
        edges (mapv #(str/split (name (first %)) #"-") distances-sorted)


        parent (atom {})
        size (atom {})

        find-root (fn find-root [x]
                    (let [p (get @parent x x)]
                      (if (= p x)
                        x
                        (let [root (find-root p)]
                          (swap! parent assoc x root)
                          root))))

        union (fn [x y]
                (let [root-x (find-root x)
                      root-y (find-root y)]
                  (when (not= root-x root-y)
                    (let [size-x (get @size root-x 1)
                          size-y (get @size root-y 1)
                          new-size (+ size-x size-y)]
                      (swap! parent assoc root-x root-y)
                      (swap! size assoc root-y new-size)
                      new-size))))

        needed-connection-pos (loop [edge-count 0
                                     remaining-edges edges]
                                (if (empty? remaining-edges)
                                  edge-count
                                  (let [[a b] (first remaining-edges)
                                        new-size (union a b)]
                                    (if (= new-size total-coordinates)
                                      (inc edge-count)
                                      (recur (inc edge-count) (rest remaining-edges))))))
        circuit-needed (str/split (name (key (last (take needed-connection-pos distances-sorted)))) #"-")]

    ; THIS WAS MY SOLUTION BUT IT WAAAAAAAY TO SLOW SO I ASSISTED MY SELF TO SEA ABOUT Unions and Find roots to make it faster
    ; at the end it was 4710 I solved first manually with my circuits function above and see if it gave me the correct coordinates
    ;(loop [circuits []
    ;       edge-count 0
    ;       remaining-edges edges]
    ;  (println edge-count)
    ;  ;(println (map count circuits))
    ;  (if (empty? remaining-edges)
    ;    edge-count
    ;    (let [current-edge (first remaining-edges)
    ;          new-circuits (reduce
    ;                         (fn [groups circuit]
    ;                           (let [connected (filter #(seq (clojure.set/intersection (set %) (set circuit))) groups)
    ;                                 not-connected (remove (set connected) groups)]
    ;                             (if (empty? connected)
    ;                               (conj groups circuit)
    ;                               (conj not-connected (vec (distinct (flatten (conj connected circuit))))))))
    ;                         [] (conj circuits current-edge))
    ;          largest-component (apply max (map count new-circuits))]
    ;      (if (= largest-component total-coordinates)
    ;        (inc edge-count)
    ;        (recur (conj circuits current-edge)
    ;               (inc edge-count)
    ;               (rest remaining-edges))))))

    ; TODO Get the two nodes
    circuit-needed))
