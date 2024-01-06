(ns day01a.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn usage []
  (println "usage: lein run <file>")
  (System/exit 1))

(defn process [contents]
  (let [digits ["0" "1" "2" "3" "4" "5" "6" "7" "8" "9"]]
    (reduce +
            (for [line (str/split contents #"\n")]
              (let [min-index (reduce (fn [min-index digit]
                                         (if-let [left-index (str/index-of line digit)]
                                           (if (or (nil? min-index) (< left-index min-index))
                                             left-index
                                             min-index)
                                           min-index))
                                       nil
                                       digits)
                    max-index (reduce (fn [max-index digit]
                                         (if-let [right-index (str/last-index-of line digit)]
                                           (if (or (nil? max-index) (> right-index max-index))
                                             right-index
                                             max-index)
                                           max-index))
                                       nil
                                       digits)
                    left-digit (if (nil? min-index) \x (get line min-index))
                    right-digit (if (nil? max-index) \x (get line max-index))]
                (+ (* (- (int left-digit) (int \0)) 10)
                   (- (int right-digit) (int \0))))))))

(defn -main [& args]
  (if (< (count args) 1)
    (usage))
  (let [filename (first args)
        contents (slurp filename)
        result (process contents)]
    (println "result = " result)))
