(ns advent-of-code-2025.day11
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn get-formatted-rack
  [rack]
  (into {} (map (fn [r] {(keyword (first r))
                         (str/split (str/trim (second r)) #"\s")}) rack)))

(defn get-rack-data
  []
  (->> (io/resource "inputs/day11.txt")
       slurp
       str/split-lines
       (map #(str/split % #":"))
       get-formatted-rack))

(defn rack-connections
  []
  (let [rack (get-rack-data)]
    (clojure.pprint/pprint rack)
    (count
      (loop [conn-to-process (:you rack)
             added-conn 0]
        ;(println conn-to-process)
        ;(println (filter #(not= % "out") conn-to-process))
        (if (empty? (filter #(not= % "out") conn-to-process))
          conn-to-process
          (let [next-conn (for [ctp conn-to-process]
                            (if (not= ctp "out")
                              (get rack (keyword ctp))
                              ctp))]
            ;(Thread/sleep 2000)
            (recur (flatten next-conn) added-conn)))))))

; Not working yet
(defn rack-connections-2
  []
  (let [rack (get-rack-data)]
    (clojure.pprint/pprint rack)
    (let [paths (loop [conn-to-process (:svr rack)
                       paths [conn-to-process]]
                  (println paths)
                  ;(println (filter #(not= % "out") conn-to-process))
                  (if (empty? (filter #(not= % "fft") conn-to-process))
                    paths
                    (let [next-conn (for [ctp conn-to-process]
                                      (if (not= ctp "fft")
                                        (get rack (keyword ctp))
                                        ctp))
                          flat-conns (flatten next-conn)
                          new-paths (conj paths flat-conns)]
                      (Thread/sleep 2000)
                      (recur flat-conns new-paths))))
          ;max-length (apply max (map count paths))
          ;padded-paths (mapv #(vec (take max-length (concat % (repeat "")))) paths)
          ;the-real-paths (apply map vector padded-paths)
          ]
      paths)))
