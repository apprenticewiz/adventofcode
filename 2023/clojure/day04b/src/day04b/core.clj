(ns day04b.core
  (:require [clojure.string :as str])
  (:require [clojure.set])
  (:gen-class))

(defn usage []
  (println "usage: lein run <file>")
  (System/exit 1))

(defn process [contents]
  (let [instances
          (reduce
            (fn [prev-instances line]
              (let [card-str (nth (str/split line #":\s+") 0)
                    card-num (Integer/parseInt (nth (str/split card-str #"\s+") 1))
                    rest (nth (str/split line #":\s+") 1)
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
                (reduce
                  (fn [prev-inst i]
                    (let [copies (+ (get prev-inst i 0) 1 (get prev-inst card-num 0))]
                      (assoc prev-inst i copies)))
                  prev-instances
                  (range (+ card-num 1) (+ card-num common-count 1)))))
            {}
            (str/split-lines contents))]
    (+
      (count (str/split-lines contents))
      (reduce + 0 (vals instances)))))

(defn -main [& args]
  (if (< (count args) 1)
    (usage))
  (let [filename (first args)
        contents (slurp filename)
        result (process contents)]
    (printf "result = %d\n" result)))
