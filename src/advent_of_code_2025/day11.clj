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
        (println conn-to-process)
        ;(println (filter #(not= % "out") conn-to-process))
        (if (empty? (filter #(not= % "out") conn-to-process))
          conn-to-process
          (let [next-conn (for [ctp conn-to-process]
                            (if (not= ctp "out")
                              (get rack (keyword ctp))
                              ctp))]
            ;(Thread/sleep 2000)
            (recur (flatten next-conn) added-conn)))))))

(defn make-count-paths-fn [rack]
  (let [memo (atom {})
        count-paths (fn count-paths [start end]
                      (if-let [cached (@memo [start end])]
                        cached
                        (let [result (if (= start end)
                                       1
                                       (let [connections (get rack (keyword start) [])]
                                         (reduce + (map #(count-paths % end) connections))))]
                          (swap! memo assoc [start end] result)
                          result)))]
    count-paths))

(defn rack-connections-2
  []
  (let [rack (get-rack-data)
        ;(clojure.pprint/pprint rack)
        count-paths-memo (make-count-paths-fn rack)
        paths_svr_fft (count-paths-memo "svr" "fft")
        ;_ (println paths_svr_fft)
        paths_fft_dac (count-paths-memo "fft" "dac")
        ;_ (println paths_fft_dac)
        paths_dac_out (count-paths-memo "dac" "out")
        ;_ (println paths_dac_out)
        ]
    (apply * [paths_svr_fft paths_fft_dac paths_dac_out])))
