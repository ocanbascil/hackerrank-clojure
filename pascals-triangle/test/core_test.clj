; Generic test runner
; Put inputs & outputs in resources folder with
; matching index suffixes like input_1.txt, output_1.txt

(ns core-test
  (:require [clojure.test :refer :all]
            [core :refer :all]))

; input - output tuples to run the tests against
(def tups
    (->> (clojure.java.io/file "test/resources")
        file-seq
        rest
        (group-by #(re-find #"\d" (.getName %)))
        vals
        (map #(map slurp %))
        (map #(map clojure.string/split-lines %))))


(deftest test-solution
  (testing "Inputs & outputs"
    (let [results (->> tups
            (map first)
            (map solution))
          tups (->> tups
            (map second)
            (interleave results)
            (partition 2)
            (map #(apply = %)))]
          (do println tups)
          (is (every? identity tups)))))