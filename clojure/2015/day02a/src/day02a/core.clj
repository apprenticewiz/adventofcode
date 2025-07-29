(ns day02a.core
  (:gen-class))

(require '[clojure.java.io :as io]
         '[clojure.string :as string])

(defn usage []
  (binding [*out* *err*]
    (println "usage: lein run <input file>"))
  (System/exit 1))

(defn calc-area [acc line]
  (let [[l w h] (map #(Integer/parseInt %) (string/split line #"x"))
        area1 (* l w)
        area2 (* l h)
        area3 (* w h)
        surface-area (+ (* 2 area1) (* 2 area2) (* 2 area3))
        min-area (min area1 area2 area3)]
    (+ acc surface-area min-area)))

(defn process [content]
  (let [lines (string/split content #"\r?\n")]
    (reduce calc-area 0 lines)))

(defn -main [& args]
    (if (= 1 (count args))
      (let [filename (first args)
            content (slurp filename)
            result (process content)]
        (println (str "result = " result)))
    (usage)))
