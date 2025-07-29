(ns day03b.core
  (:gen-class))

(require '[clojure.java.io :as io])

(defn usage []
  (binding [*out* *err*]
    (println "usage: lein run <input file>"))
  (System/exit 1))

(defn track-positions [[santa robo-santa santa-move positions] ch]
  (if santa-move
    (let [santa' (case ch
                   \^ (update santa 1 inc)
                   \v (update santa 1 dec)
                   \< (update santa 0 dec)
                   \> (update santa 0 inc)
                   santa)
          positions' (conj positions santa')]
      [santa' robo-santa false positions'])
    (let [robo-santa' (case ch
                        \^ (update robo-santa 1 inc)
                        \v (update robo-santa 1 dec)
                        \< (update robo-santa 0 dec)
                        \> (update robo-santa 0 inc)
                        robo-santa)
        positions' (conj positions robo-santa')]
      [santa robo-santa' true positions'])))

(defn process [content]
  (let [[_ _ _ positions] (reduce track-positions [[0 0] [0 0] true #{}] content)]
    (count positions)))

(defn -main [& args]
    (if (= 1 (count args))
      (let [filename (first args)
            content (slurp filename)
            result (process content)]
        (println (str "result = " result)))
    (usage)))
