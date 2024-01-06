(ns day01b.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn usage []
  (println "usage: lein run <file>")
  (System/exit 1))

(defn process [contents]
  (let [digit-strs {"0" \0 "1" \1 "2" \2 "3" \3 "4" \4 "5" \5 "6" \6 "7" \7 "8" \8 "9" \9
                    "zero" \0 "one" \1 "two" \2 "three" \3 "four" \4 "five" \5
                    "six" \6 "seven" \7 "eight" \8 "nine" \9}]
    (reduce +
            (for [line (str/split-lines contents)]
              (let [min-pair (reduce (fn [min-pair digit-str]
                                        (let [min-index (first min-pair)
                                              min-digit (second min-pair)]
                                          (if-let [left-index (str/index-of line digit-str)]
                                            (if (or (nil? min-index) (< left-index min-index))
                                              (list left-index (get digit-strs digit-str))
                                              (list min-index min-digit))
                                            (list min-index min-digit))))
                                        (list nil nil)
                                        (keys digit-strs))
                    max-pair (reduce (fn [max-pair digit-str]
                                        (let [max-index (first max-pair)
                                              max-digit (second max-pair)]
                                          (if-let [right-index (str/last-index-of line digit-str)]
                                            (if (or (nil? max-index) (> right-index max-index))
                                              (list right-index (get digit-strs digit-str))
                                              (list max-index max-digit))
                                            (list max-index max-digit))))
                                        (list nil nil)
                                        (keys digit-strs))
                    left-digit (second min-pair)
                    right-digit (second max-pair)]
                (+ (* (- (int left-digit) (int \0)) 10)
                   (- (int right-digit) (int \0))))))))

(defn -main [& args]
  (if (< (count args) 1)
    (usage))
  (let [filename (first args)
        contents (slurp filename)
        result (process contents)]
    (println "result = " result)))
