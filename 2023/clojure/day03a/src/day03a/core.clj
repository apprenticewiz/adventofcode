(ns day03a.core
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

(defn build-parts [contents]
  (nth (reduce
    (fn [[row part-locs] line]
      [(+ row 1) (nth (reduce
        (fn [[col part-locs] ch]
            (if (and (not (Character/isDigit ch)) (not= ch \.))
              [(+ col 1) (assoc part-locs (->Position row col) ch)]
              [(+ col 1) part-locs]))
        [0 part-locs]
        line) 1)])
    [0 {}]
    (str/split-lines contents)) 1))

(defn check-parts [number-locs part-locs]
  (reduce
    (fn [result number-loc]
      (let [number-row (-> number-loc :row)
            number-col-first (-> number-loc :col)
            number-col-last (+ (-> number-loc :col) (count (get number-locs number-loc)))
            found-adjacent (reduce
              (fn [found number-col]
                (if found
                  true
                  (reduce
                    (fn [found-adjacent neighbor]
                      (let [adjacent-pos (->Position (+ (-> number-loc :row) (-> neighbor :row))
                                                     (+ number-col (-> neighbor :col)))]
                        (if (contains? part-locs adjacent-pos)
                          true
                          found-adjacent)))
                    false
                    [(->Position -1 -1) (->Position -1 0) (->Position -1 1)
                     (->Position 0 -1) (->Position 0 1)
                     (->Position 1 -1) (->Position 1 0) (->Position 1 1)])))
              false
              (range number-col-first number-col-last)
            )]
        (if found-adjacent
          (+ result (Integer/parseInt (get number-locs number-loc)))
          result)))
    0
    (keys number-locs)))

(defn process [contents]
  (let [number-locs (build-numbers contents)
        part-locs (build-parts contents)]
    (check-parts number-locs part-locs)))

(defn -main [& args]
  (if (< (count args) 1)
    (usage))
  (let [filename (first args)
        contents (slurp filename)
        result (process contents)]
    (printf "result = %d\n" result)))
