(ns core)

(defn pascal [n]
  (case n
    1 '(1)
    2 '(1 1)
    (let [m (partition 2 1 (pascal (dec n)))]
      (concat [1] (map #(apply + %) m) [1]))))

(defn solution [[n & _]]
  (->> n
    Integer/parseInt
    inc
    (range 1)
    (map pascal)
    (map #(clojure.string/join " " %))))
