(ns day03b.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn usage []
  (println "usage: lein run <file>")
  (System/exit 1))

(defrecord Position [row col])

(defrecord NumberLocation [number pos])

(defrecord GearLocation [gear pos])

(defn build-numbers [contents]
  (let [number-str (atom "")
        scanning-number (atom false)
        number-loc (atom nil)
        number-locs (atom [])]
    (doseq [[row line] (map-indexed vector (str/split-lines contents))]
      (doseq [[col ch] (map-indexed vector line)]
        (if @scanning-number
          (if (Character/isDigit ch)
            (swap! number-str str ch)
            (do
              (reset! number-loc (assoc @number-loc :number @number-str))
              (swap! number-locs conj @number-loc)
              (reset! number-loc nil)
              (reset! number-str "")
              (reset! scanning-number false)))
          (do
            (if (Character/isDigit ch)
              (do
                (reset! scanning-number true)
                (swap! number-str str ch)
                (reset! number-loc (assoc @number-loc :pos (->Position row col))))))))
      (if @scanning-number
        (do
          (reset! number-loc (assoc @number-loc :number @number-str))
          (swap! number-locs conj @number-loc)
          (reset! number-loc nil)
          (reset! number-str "")
          (reset! scanning-number false))))
    @number-locs))

(defn build-gears [contents]
  (let [gear-locs (atom [])]
    (doseq [[row line] (map-indexed vector (str/split-lines contents))]
      (doseq [[col ch] (map-indexed vector line)]
        (if (and (not (Character/isDigit ch))
                 (not (= ch \.)))
          (swap! gear-locs conj (->GearLocation ch (->Position row col))))))
    @gear-locs))

(defn check-gears [number-locs gear-locs]
  (let [result (atom 0)]
    (doseq [gear-loc gear-locs]
      (let [adjacents (atom [])]
        (doseq [number-loc number-locs]
          (let [skip (atom false)]
            (doseq [delta-row (range -1 2)]
              (let [adjacent-row (+ (-> gear-loc :pos :row) delta-row)]
                (doseq [delta-col (range -1 2)]
                  (let [adjacent-col (+ (-> gear-loc :pos :col) delta-col)
                        number-col-start (-> number-loc :pos :col)
                        number-col-end (+ (-> number-loc :pos :col) (count (-> number-loc :number)))]
                    (doseq [number-col (range number-col-start number-col-end)]
                      (if (and (not @skip)
                               (= adjacent-row (-> number-loc :pos :row))
                               (= adjacent-col number-col))
                        (do
                          (reset! skip true)
                          (swap! adjacents conj (Integer/parseInt (-> number-loc :number))))))))))))
        (if (= (count @adjacents) 2)
          (swap! result + (reduce * @adjacents)))))
    @result))

(defn process [contents]
  (let [number-locs (build-numbers contents)
        gear-locs (build-gears contents)]
    (check-gears number-locs gear-locs)))

(defn -main [& args]
  (if (< (count args) 1)
    (usage))
  (let [filename (first args)
        contents (slurp filename)
        result (process contents)]
    (printf "result = %d\n" result)))
