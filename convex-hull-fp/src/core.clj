(ns core)

(defn sq [x] (* x x))

(defn parse-int [s] (Integer/parseInt s))

(defn dist [[x1 y1][x2 y2]]
  (Math/sqrt
   (+
    (sq (- x1 x2))
    (sq (- y1 y2)))))

(defn polygon-perimeter [v]
  (let [coords (conj v (first v))
        tups (partition 2 1 coords)]
    (reduce + (map #(apply dist %) tups))))

(defn y-coord-sort [points]
  (sort-by (fn [[a b]] [b a]) points))

(defn angle-sort [[[x0 y0] & r]]
  (cons [x0 y0]
          (sort-by (fn [[x y]]
             (Math/atan2 (- y y0) (- x x0))) r)))

(defn right-turn? [[x1 y1][x2 y2][x3 y3]]
  (neg? (-
         (* (- x2 x1)(- y3 y1))
         (* (- y2 y1)(- x3 x1)))))

(defn graham-step [acc item]
  "http://en.wikipedia.org/wiki/Graham_scan"
  (let [acc (vec acc)
        subv (subvec acc (- (count acc) 2))]
    (if (apply right-turn? (conj subv item))
      (graham-step (pop acc) item)
      (conj acc item))))

(defn graham-scan [v]
  (let [[f s & r] (-> v y-coord-sort angle-sort)]
    (reduce graham-step [f s] r)))


(defn solution [v]
  (->> v
       rest
       (map #(clojure.string/split % #" "))
       (map #(map parse-int %))
       graham-scan
       polygon-perimeter
       (format "%.1f")
       vector))

; Uncomment the lines below to run on hackerrank
;(let [num-coords (parse-int(read-line))
;      coords (doall (repeatedly num-coords read-line))
;      solution (solution (concat [nil] coords))]
;      (doall (map println solution)))
