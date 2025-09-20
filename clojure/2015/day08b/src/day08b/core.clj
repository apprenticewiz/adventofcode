(ns day08b.core
  (:gen-class))

(require '[clojure.java.io :as io]
         '[clojure.string :as string])

(defn usage []
  (binding [*out* *err*]
    (println "usage: lein run <input file>"))
  (System/exit 1))

(defn scan-helper [i charseq]
  (cond (empty? charseq) i
        :else (let [ch (first charseq)]
                (cond (= ch \\) (scan-helper (+ 2 i) (rest charseq))
                      (= ch \") (scan-helper (+ 2 i) (rest charseq))
                      :else (scan-helper (+ 1 i) (rest charseq))))))

(defn scan-line [acc line]
  (let [code-len (count line)
        enc-len (scan-helper 0 line)]
    (+ acc 2 (- enc-len code-len))))

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
