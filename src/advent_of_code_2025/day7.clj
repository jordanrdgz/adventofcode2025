(ns advent-of-code-2025.day7
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn get-diagram
  []
  (->> (io/resource "inputs/day7.txt")
       slurp
       str/split-lines
       (map vec)))

(defn is-space?
  [c]
  (= c \.))

(defn is-splitter
  [c]
  (= c \^))

(defn beam-splitting
  []
  (let [diagram (get-diagram)
        counters (-> (first diagram)
                     count
                     (take (repeat 0))
                     vec)
        [_ splits paths new-diagram]
        (reduce
          (fn [[beam-positions split-count counters new-diagram] row]
            (if (empty? beam-positions)
              ;Search for S first
              (let [start-pos (first
                                (keep-indexed
                                  (fn [idx element]
                                    (when (= element \S)
                                      idx))
                                  row))]
                [[start-pos]
                 split-count
                 (update counters start-pos inc)
                 (conj new-diagram row)])
              ;All other walls
              (let [[splits new-counters new-row]
                    (reduce
                      (fn [[times-split counters current-row] index]
                        (let [element (row index)]
                          (if (is-space? element)
                            [times-split counters (assoc current-row index \|)]
                            (if (is-splitter element)
                              [(inc times-split)
                               (-> (update counters (dec index) #(+ (counters index) %))
                                   (assoc index 0)
                                   (update (inc index) #(+ (counters index) %)))
                               (-> current-row
                                   (assoc (dec index) \|)
                                   (assoc (inc index) \|))]
                              [times-split counters current-row]))))
                      [0 counters row] beam-positions)
                    new-beam-positions (keep-indexed
                                         (fn [idx element]
                                           (when (= element \|)
                                             idx))
                                         new-row)]
                [new-beam-positions
                 (+ split-count splits)
                 new-counters
                 (conj new-diagram new-row)])))
          [[] 0 counters []] diagram)]
    (clojure.pprint/pprint (map #(apply str %) new-diagram))
    (println "TIMES SPLIT: " splits)
    (println "TOTAL PATHS: " (apply + paths))))
