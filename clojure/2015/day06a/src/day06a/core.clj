(ns day06a.core
  (:gen-class))

(require '[clojure.java.io :as io]
         '[clojure.string :as string])

(defn usage []
  (binding [*out* *err*]
    (println "usage: lein run <input file>"))
  (System/exit 1))

(defn perform [grid action r1 c1 r2 c2]
  (reduce
    (fn [curr-grid [row col]]
      (case action
        :turn-on (assoc curr-grid [row col] true)
        :turn-off (assoc curr-grid [row col] false)
        :toggle (assoc curr-grid [row col] (not (grid [row col] false)))))
    grid
    (for [row (range r1 (inc r2))
          col (range c1 (inc c2))]
      [row col])))

(defn sum [grid]
  (reduce
    (fn [total [row col]]
      (+ total (if (grid [row col] false) 1 0)))
    0
    (for [row (range 0 1000)
          col (range 0 1000)]
      [row col])))

(defn process [content]
  (let [re #"(turn on|turn off|toggle) (\d+),(\d+) through (\d+),(\d+)"
        lines (string/split-lines content)
        grid (reduce
               (fn [curr-grid line]
                 (let [[_ saction sr1 sc1 sr2 sc2] (re-matches re line)
                       action (case saction
                                "turn on" :turn-on
                                "turn off" :turn-off
                                "toggle" :toggle)
                       r1 (Integer/parseInt sr1)
                       c1 (Integer/parseInt sc1)
                       r2 (Integer/parseInt sr2)
                       c2 (Integer/parseInt sc2)]
                  (perform curr-grid action r1 c1 r2 c2)))
               {}
               lines)]
    (sum grid)))

(defn -main [& args]
    (if (= 1 (count args))
      (let [filename (first args)
            content (slurp filename)
            result (process content)]
        (println (str "result = " result)))
    (usage)))
