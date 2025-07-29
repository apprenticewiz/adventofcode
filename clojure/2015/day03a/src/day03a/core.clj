(ns day03a.core
  (:gen-class))

(require '[clojure.java.io :as io])

(defn usage []
  (binding [*out* *err*]
    (println "usage: lein run <input file>"))
  (System/exit 1))

(defn track-positions [[santa positions] ch]
  (let [santa' (case ch
                 \^ (update santa 1 inc)
                 \v (update santa 1 dec)
                 \< (update santa 0 dec)
                 \> (update santa 0 inc)
                 santa)
        positions' (conj positions santa')]
    [santa' positions']))


(defn process [content]
  (let [[_ positions] (reduce track-positions [[0 0] #{}] content)]
    (count positions)))

(defn -main [& args]
    (if (= 1 (count args))
      (let [filename (first args)
            content (slurp filename)
            result (process content)]
        (println (str "result = " result)))
    (usage)))
