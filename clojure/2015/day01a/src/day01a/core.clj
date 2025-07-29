(ns day01a.core
  (:gen-class))

(require '[clojure.java.io :as io])

(defn usage []
  (binding [*out* *err*]
    (println "usage: lein run <input file>"))
  (System/exit 1))

(defn count-parens [acc ch]
  (cond
    (= ch \() (inc acc)
    (= ch \)) (dec acc)
    :else acc))

(defn process [content]
  (reduce count-parens 0 content))

(defn -main [& args]
    (if (= 1 (count args))
      (let [filename (first args)
            content (slurp filename)
            result (process content)]
        (println (str "result = " result)))
    (usage)))
