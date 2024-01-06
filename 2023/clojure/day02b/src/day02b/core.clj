(ns day02b.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn usage []
  (println "usage: lein run <file>")
  (System/exit 1))

(defn process [contents]
  (let [result (atom 0)]
    (doseq [line (str/split-lines contents)]
      (when-let [[_ reveals-str] (str/split line #": ")]
        (let [red-needed (atom 0)
              green-needed (atom 0)
              blue-needed (atom 0)]
          (doseq [subset-str (str/split reveals-str #"; ")]
            (doseq [cubes-str (str/split subset-str #", ")]
              (when-let [[amount-str color] (str/split cubes-str #"\s")]
                (let [amount (Integer/parseInt amount-str)]
                  (case color
                    "red" (if (> amount @red-needed) (reset! red-needed amount))
                    "green" (if (> amount @green-needed) (reset! green-needed amount))
                    "blue" (if (> amount @blue-needed) (reset! blue-needed amount))
                    (throw (Exception. "unknown color")))))))
          (reset! result (+ @result (* @red-needed @green-needed @blue-needed))))))
    @result))

(defn -main [& args]
  (if (< (count args) 1)
    (usage))
  (let [filename (first args)
        contents (slurp filename)
        result (process contents)]
    (println "result = " result)))
