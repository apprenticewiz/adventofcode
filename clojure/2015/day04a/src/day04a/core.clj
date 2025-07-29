(ns day04a.core
  (:gen-class))

(require '[clojure.java.io :as io]
         '[clojure.string :as string])

(import [java.security MessageDigest])

(defn usage []
  (binding [*out* *err*]
    (println "usage: lein run <key>"))
  (System/exit 1))

(defn process [key-val]
  (let [md5 (MessageDigest/getInstance "MD5")]
    (loop [i 1]
      (let [try-key (str key-val i)
            digest-bytes (.digest md5 (.getBytes try-key "UTF-8"))
            hex-digest (apply str (map #(format "%02x" (bit-and % 0xff)) digest-bytes))]
        (if (string/starts-with? hex-digest "00000")
          i
          (recur (inc i)))))))

(defn -main [& args]
    (if (= 1 (count args))
      (let [key-val (first args)
            result (process key-val)]
        (println (str "result = " result)))
    (usage)))
