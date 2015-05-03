(ns core)

(defn solution [[num-words & words]]
  (letfn [(rotate [[f & r]] (conj (vec r) f))
          (rotate-str [s] (reduce str (rotate s)))
          (rotations [s]
                     (->> s
                          (iterate rotate-str)
                          (take (count s))
                          rotate
                          (clojure.string/join " ")))]
    (map rotations words)))


; Uncomment the lines below to run on hackerrank
;(let [num-lines (Integer/parseInt (read-line))
;      lines (doall (repeatedly num-lines read-line))
;      solution (solution (concat [nil] lines))]
;      (doall (map println solution)))