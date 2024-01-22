(ns day03b.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn usage []
  (println "usage: lein run <file>")
  (System/exit 1))

(defrecord Position [row col])

(defn build-numbers [contents]
  (nth (reduce
    (fn [[row num-locs] line]
      [(+ row 1) 
      (nth (reduce
          (fn [[col scanning-number number current-pos num-locs] ch]
            (if scanning-number
              (if (Character/isDigit ch)
                (if (= (+ col 1) (count line))
                  [(+ col 1) false "" (->Position -1 -1) (assoc num-locs current-pos (str number ch))]
                  [(+ col 1) true (str number ch) current-pos num-locs])
                [(+ col 1) false "" (->Position -1 -1) (assoc num-locs current-pos number)])
              (if (Character/isDigit ch)
                [(+ col 1) true (str ch) (->Position row col) num-locs]
                [(+ col 1) false "" (->Position -1 -1) num-locs])))
          [0 false "" (->Position -1 -1) num-locs]
          line) 4)])
    [0 {}]
    (str/split-lines contents)) 1))

(defn build-gears [contents]
  (nth (reduce
    (fn [[row part-locs] line]
      [(+ row 1) (nth (reduce
        (fn [[col part-locs] ch]
            (if (= ch \*)
              [(+ col 1) (assoc part-locs (->Position row col) ch)]
              [(+ col 1) part-locs]))
        [0 part-locs]
        line) 1)])
    [0 {}]
    (str/split-lines contents)) 1))

(defn check-gears [number-locs gear-locs]
  (reduce
    (fn [result gear-loc]
      (let [adjacents (reduce
          (fn [adjacents-vec number-loc]
            (let [number-row (-> number-loc :row)
                  number-col-first (-> number-loc :col)
                  number-col-last (+ (-> number-loc :col) (count (get number-locs number-loc)))
                  found-adjacent (reduce
                    (fn [found neighbor]
                      (let [adjacent-pos (->Position (+ (-> gear-loc :row) (-> neighbor :row))
                                                     (+ (-> gear-loc :col) (-> neighbor :col)))]
                        (if (and (= (-> adjacent-pos :row) number-row)
                                 (>= (-> adjacent-pos :col) number-col-first)
                                 (< (-> adjacent-pos :col) number-col-last))
                          true
                          found)))
                    false
                    [(->Position -1 -1) (->Position -1 0) (->Position -1 1)
                     (->Position 0 -1) (->Position 0 1)
                     (->Position 1 -1) (->Position 1 0) (->Position 1 1)])]
              (if found-adjacent
                (conj adjacents-vec (get number-locs number-loc))
                adjacents-vec)))
          []
          (keys number-locs))]
        (if (= (count adjacents) 2)
          (+ result (reduce * 1 (map (fn [n] (Integer/parseInt n)) adjacents)))
          result)))
    0
    (keys gear-locs)))

(defn process [contents]
  (let [number-locs (build-numbers contents)
        gear-locs (build-gears contents)]
    (check-gears number-locs gear-locs)))

(defn -main [& args]
  (if (< (count args) 1)
    (usage))
  (let [filename (first args)
        contents (slurp filename)
        result (process contents)]
    (printf "result = %d\n" result)))
