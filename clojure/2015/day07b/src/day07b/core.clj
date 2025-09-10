(ns day07b.core
  (:gen-class))

(require '[clojure.java.io :as io]
         '[clojure.string :as string])

(defn usage []
  (binding [*out* *err*]
    (println "usage: lein run <input file>"))
  (System/exit 1))

(defn evaluate [ops cache expr]
  (cond (re-matches #"^\d+$" expr) [(Integer/parseInt expr) cache]
        (contains? cache expr) [(get cache expr) cache]
        :else
          (let [op (get ops expr)
                [r new-cache] (case (first op)
                                :assign (let [[_ src] op]
                                          (evaluate ops cache src))
                                :not (let [[_ src] op
                                           [a c] (evaluate ops cache src)]
                                       [(bit-not a) c])
                                :and (let [[_ src1 src2] op
                                           [a ca] (evaluate ops cache src1)
                                           [b cb] (evaluate ops ca src2)]
                                       [(bit-and a b) cb])
                                :or (let [[_ src1 src2] op
                                          [a ca] (evaluate ops cache src1)
                                          [b cb] (evaluate ops ca src2)]
                                      [(bit-or a b) cb])
                                :lshift (let [[_ src amt] op
                                              [a c] (evaluate ops cache src)]
                                          [(bit-shift-left a amt) c])
                                :rshift (let [[_ src amt] op
                                              [a c] (evaluate ops cache src)]
                                          [(bit-shift-right a amt) c]))
                masked (bit-and r 65535)]
            [masked (assoc new-cache expr masked)])))

(defn process [content]
  (let [regex-handlers
          [[#"^(\d+|\w+) -> (\w+)$"
            (fn [[_ src dest]]
              [dest [:assign src]])]
           [#"^NOT (\d+|\w+) -> (\w+)$"
            (fn [[_ src dest]]
              [dest [:not src]])]
           [#"^(\d+|\w+) (AND|OR) (\d+|\w+) -> (\w+)$"
            (fn [[_ src1 op src2 dest]]
              [dest (case op
                      "AND" [:and src1 src2]
                      "OR" [:or src1 src2])])]
           [#"^(\d+|\w+) (LSHIFT|RSHIFT) (\d+) -> (\w+)$"
            (fn [[_ src op amt dest]]
              [dest (case op
                      "LSHIFT" [:lshift src (Integer/parseInt amt)]
                      "RSHIFT" [:rshift src (Integer/parseInt amt)])])]]
        lines (string/split-lines content)
        operations 
          (reduce
            (fn [acc line]
              (let [[dest op] (some (fn [[re handler]]
                                      (when-let [m (re-matches re line)]
                                        (handler m)))
                                    regex-handlers)]
                   (assoc acc dest op)))
            {}
            lines)
        [a _] (evaluate operations {} "a")
        new-ops (assoc operations "b" [:assign (str a)])]
    (first (evaluate new-ops {} "a"))))

(defn -main [& args]
    (if (= 1 (count args))
      (let [filename (first args)
            content (slurp filename)
            result (process content)]
        (println (str "result = " result)))
    (usage)))
