(ns day03a.core
  (:require [clojure.string :as str])
  (:gen-class))

(defn usage []
  (println "usage: lein run <file>")
  (System/exit 1))

(defrecord Position [row col])

(defn build-numbers [contents]
  (let [number-str (atom "")
        scanning-number (atom false)
        current-loc (atom nil)
        number-locs (atom {})]
    (doseq [[row line] (map-indexed vector (str/split-lines contents))]
      (doseq [[col ch] (map-indexed vector line)]
        (if @scanning-number
          (if (Character/isDigit ch)
            (swap! number-str str ch)
            (do
              (reset! number-locs (assoc @number-locs @current-loc @number-str))
              (reset! current-loc nil)
              (reset! number-str "")
              (reset! scanning-number false)))
          (do
            (if (Character/isDigit ch)
              (do
                (reset! scanning-number true)
                (swap! number-str str ch)
                (reset! current-loc (->Position row col)))))))
      (if @scanning-number
        (do
          (reset! number-locs (assoc @number-locs @current-loc @number-str))
          (reset! current-loc nil)
          (reset! number-str "")
          (reset! scanning-number false))))
    @number-locs))

(defn build-gears [contents]
  (let [gear-locs (atom {})]
    (doseq [[row line] (map-indexed vector (str/split-lines contents))]
      (doseq [[col ch] (map-indexed vector line)]
        (if (and (not (Character/isDigit ch))
                 (not (= ch \.)))
          (reset! gear-locs (assoc @gear-locs (->Position row col) ch)))))
    @gear-locs))

(defn check-gears [number-locs gear-locs]
  (let [result (atom 0)]
    (doseq [number-loc (keys number-locs)]
      (let [adjacent-count (atom 0)
            number-col-start (-> number-loc :col)
            number-col-end (+ (-> number-loc :col) (count (get number-locs number-loc)))]
        (doseq [number-col (range number-col-start number-col-end)]
          (doseq [delta-row (range -1 2)]
            (let [adjacent-row (+ (-> number-loc :row) delta-row)]
              (doseq [delta-col (range -1 2)]
                (let [adjacent-col (+ number-col delta-col)]
                  (doseq [gear-loc (keys gear-locs)]
                    (if (and (= adjacent-row (-> gear-loc :row))
                             (= adjacent-col (-> gear-loc :col)))
                      (reset! adjacent-count (+ @adjacent-count 1)))))))))
        (if (not (= @adjacent-count 0))
          (swap! result + (Integer/parseInt (get number-locs number-loc))))))
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
