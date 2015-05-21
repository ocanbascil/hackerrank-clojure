(ns core)

(defn parse-nums [s]
  (->> (clojure.string/split s #" ")
       (map #(Long/parseLong  %))))

(defn apetite [n]
  (fn [[a h]]
    (+ a
       (* (dec n) h))))


(defn mangoes-consumed [tups]
  (reduce + (map (apetite (count tups)) tups)))


(defn bisector [tups mangoes]
  (letfn [(subv [p n]
                (subvec tups 0 (quot (+ (count p) (count n))2)))
          (bisect
           ([p] (bisect p tups))
           ([p n]
            (if (= p n)
              (count n)
              (if (<= (mangoes-consumed n) mangoes)
                (recur n (subv n tups))
                (recur p (subv p n))))))]
    bisect))


(defn solution [v]
  (let [[[num-guests mangoes] a h] (map parse-nums v)
        tups (partition 2 (interleave a h))
        [f & _ :as tups] (vec (sort-by (apetite (count tups)) tups))
        bisect (bisector tups mangoes)]
    [(cond
      (> (mangoes-consumed [f]) mangoes) 0
      (<= (mangoes-consumed tups) mangoes) (count tups)
      :else (bisect [f]))]))


; Uncomment the lines below to run on hackerrank
; (let [inputs (doall (repeatedly 3 read-line))]
;     (doall (map println (solution inputs))))