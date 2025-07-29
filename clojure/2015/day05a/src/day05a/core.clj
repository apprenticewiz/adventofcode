(ns day05a.core
  (:gen-class))

(require '[clojure.java.io :as io]
         '[clojure.string :as string])

(defn usage []
  (binding [*out* *err*]
    (println "usage: lein run <input file>"))
  (System/exit 1))

(defn prop1 [s]
  (let [num-vowels (count (re-seq #"[aeiou]" s))]
    (>= num-vowels 3)))

(defn prop2 [s]
  (re-find #"(.)\1" s))

(defn prop3 [s]
  (not (re-find #"ab|cd|pq|xy" s)))

(defn check-line [acc line]
  (if (and (prop1 line) (prop2 line) (prop3 line))
    (inc acc)
    acc))

(defn process [content]
  (let [lines (string/split content #"\r?\n")]
    (reduce check-line 0 lines)))

(defn -main [& args]
    (if (= 1 (count args))
      (let [filename (first args)
            content (slurp filename)
            result (process content)]
        (println (str "result = " result)))
    (usage)))
