(ns day04a.core
  (:require [clojure.string :as str])
  (:require [clojure.set])
  (:gen-class))

(defn usage []
  (println "usage: lein run <file>")
  (System/exit 1))

(defn process [contents]
  (reduce
    (fn [result line]
      (let [rest (nth (str/split line #":\s+") 1)
            winning-str (nth (str/split rest #"\s+\|\s+") 0)
            winning-set
              (reduce
                (fn [prev-set num-str]
                  (conj prev-set (Integer/parseInt num-str)))
                #{}
                (str/split winning-str #"\s+"))
            hand-str (nth (str/split rest #"\s+\|\s+") 1)
            hand-set
              (reduce
                (fn [prev-set num-str]
                  (conj prev-set (Integer/parseInt num-str)))
                #{}
                (str/split hand-str #"\s+"))
            intersection (clojure.set/intersection winning-set hand-set)
            common-count (count intersection)]
        (if (> common-count 0)
          (+ result (bit-shift-left 1 (- common-count 1)))
          result)))
    0
    (str/split-lines contents)))

(defn -main [& args]
  (if (< (count args) 1)
    (usage))
  (let [filename (first args)
        contents (slurp filename)
        result (process contents)]
    (printf "result = %d\n" result)))
