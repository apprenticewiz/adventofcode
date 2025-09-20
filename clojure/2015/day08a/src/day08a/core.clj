(ns day08a.core
  (:gen-class))

(require '[clojure.java.io :as io]
         '[clojure.string :as string])

(defn usage []
  (binding [*out* *err*]
    (println "usage: lein run <input file>"))
  (System/exit 1))

(defn scan-quoted [i quoted]
  (cond (empty? quoted) i
        (= 1 (count quoted)) (+ i 1)
        :else (let [ch1 (nth quoted 0)
                     ch2 (nth quoted 1)]
                 (if (= ch1 \\)
                   (cond (= ch2 \\) (scan-quoted (+ i 1) (drop 2 quoted))
                         (= ch2 \") (scan-quoted (+ i 1) (drop 2 quoted))
                         (= ch2 \x) (scan-quoted (+ i 1) (drop 4 quoted))
                         :else (scan-quoted (+ i 1) (rest quoted)))
                   (scan-quoted (+ i 1) (rest quoted))))))

(defn scan-line [acc line]
  (let [code-len (count line)
        quoted (rest (butlast (seq line)))
        mem-len (scan-quoted 0 quoted)]
    (+ acc (- code-len mem-len))))

(defn process [content]
  (let [lines (string/split content #"\r?\n")]
    (reduce scan-line 0 lines)))

(defn -main [& args]
    (if (= 1 (count args))
      (let [filename (first args)
            content (slurp filename)
            result (process content)]
        (println (str "result = " result)))
    (usage)))
