(ns core)

(defn gcd [a b]
      (if (zero? b)
        a
        (recur b (mod a b))))

(defn lcm [a b]
  (/ (*' a b) (gcd a b)))

(defn solution [[_ bunnies]]
  (->> (clojure.string/split bunnies #" ")
       (map #(Integer/parseInt %))
       (reduce lcm)
       str
       vector))

; Uncomment the lines below to run on hackerrank
;(let [_ (read-line)
;      bunnies (read-line)
;      solution (solution [nil bunnies])]
;      (doall (map println solution)))