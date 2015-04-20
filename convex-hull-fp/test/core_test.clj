; Generic test runner
; Put inputs & outputs in resources folder with
; matching index suffixes like input_1.txt, output_1.txt
; solution function should accept a seq and return a seq

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


; Converts solution results to string and compares it to the expected output
(deftest test-solution
  (testing "Inputs & outputs"
    (let [results (->> tups
            (map first)
            (map solution)
            (map #(map str %)))
          tups (->> tups
            (map second)
            (interleave results)
            (partition 2)
            (map #(apply = %)))]
          (is (every? identity tups)))))