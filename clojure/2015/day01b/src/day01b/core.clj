(ns day01b.core
  (:gen-class))

(require '[clojure.java.io :as io])

(defn usage []
  (binding [*out* *err*]
    (println "usage: lein run <input file>"))
  (System/exit 1))

(defn check-pos [[pos floor] ch]
  (if (neg? floor)
    [pos floor]
    (let [floor' (case ch
                   \( (inc floor)
                   \) (dec floor)
                   floor)]
      [(inc pos) floor'])))

(defn process [content]
  (let [[pos _] (reduce check-pos [0 0] content)]
    pos))

(defn -main [& args]
    (if (= 1 (count args))
      (let [filename (first args)
            content (slurp filename)
            result (process content)]
        (println (str "result = " result)))
    (usage)))
