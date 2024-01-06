(ns day02a.core
  (:require [clojure.string :as str])
  (:gen-class))

(def TOTAL-RED 12)
(def TOTAL-GREEN 13)
(def TOTAL-BLUE 14)

(defn usage []
  (println "usage: lein run <file>")
  (System/exit 1))

(defn process [contents]
  (let [result (atom 0)]
    (doseq [line (str/split-lines contents)]
      (when-let [[game-str reveals-str] (str/split line #": ")]
        (when-let [[_ game-num-str] (str/split game-str #" ")]
          (let [game-num (Integer/parseInt game-num-str)
                valid (atom true)]
            (doseq [subset-str (str/split reveals-str #"; ")]
              (doseq [cubes-str (str/split subset-str #", ")]
                (when-let [[amount-str color] (str/split cubes-str #"\s")]
                  (let [amount (Integer/parseInt amount-str)]
                    (case color
                      "red" (if (> amount TOTAL-RED) (reset! valid false))
                      "green" (if (> amount TOTAL-GREEN) (reset! valid false))
                      "blue" (if (> amount TOTAL-BLUE) (reset! valid false))
                      (throw (Exception. "unknown color")))))))
            (when @valid
              (reset! result (+ @result game-num)))))))
    @result))

(defn -main [& args]
  (if (< (count args) 1)
    (usage))
  (let [filename (first args)
        contents (slurp filename)
        result (process contents)]
    (println "result = " result)))
