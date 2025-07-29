(ns day02b.core
  (:gen-class))

(require '[clojure.java.io :as io]
         '[clojure.string :as string])

(defn usage []
  (binding [*out* *err*]
    (println "usage: lein run <input file>"))
  (System/exit 1))

(defn calc-len [acc line]
  (let [[l w h] (map #(Integer/parseInt %) (string/split line #"x"))
        perim1 (* 2 (+ l w))
        perim2 (* 2 (+ l h))
        perim3 (* 2 (+ w h))
        present-len (min perim1 perim2 perim3)
        bow-len (* l w h)]
    (+ acc present-len bow-len)))

(defn process [content]
  (let [lines (string/split content #"\r?\n")]
    (reduce calc-len 0 lines)))

(defn -main [& args]
    (if (= 1 (count args))
      (let [filename (first args)
            content (slurp filename)
            result (process content)]
        (println (str "result = " result)))
    (usage)))
