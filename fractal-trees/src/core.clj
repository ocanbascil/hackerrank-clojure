; This question is a special case as hackerrank does not allow you to
; use def / defn, so you should copy the let block and uncomment input/output lines
(ns core)

(defn solution [[iterations & _]]
  (let [
      ; iterations (read-line)
      iterations (Integer/parseInt iterations)
      ONE \1 UNDERSCORE \_ ROWS 63 COLUMNS 100 START-LENGTH 16
      branch (fn [size] (apply str (concat [ONE] (repeat size UNDERSCORE) [ONE])))
      tree (fn [size]
             (concat
              (map str (repeat size ONE))
              (map branch (range 1 (* 2 size) 2))))
      trees (fn trees
              ([iterations] (trees iterations START-LENGTH 1))
              ([iterations length num-trees]
               (if (pos? iterations)
                 (concat [(repeat num-trees (tree length))]
                         (trees (dec iterations) (/ length 2) (* num-trees 2))))))
      line-data (fn [trees]
                  (reverse (apply concat (map #(apply map vector %) trees))))
      separator (fn [[v & _ :as all]]
                  (let [factor (* 4 (/ (count all)))
                        reps (- (* START-LENGTH factor) (count v))]
                    (apply str (repeat reps UNDERSCORE))))
      printable (fn [line-str]
                  (let [offset (/ (- COLUMNS (count line-str)) 2)
                        start (repeat (dec offset) UNDERSCORE)
                        end (repeat offset UNDERSCORE)]
                    (apply str (concat start line-str end))))
      line-str (fn [data]
                 (printable (clojure.string/join (separator data) data)))
      filler (fn [line-strings]
               (repeat (- ROWS (count line-strings)) (printable (str UNDERSCORE))))
      trees (trees iterations)
      line-data (line-data trees)
      line-strings (map line-str line-data)
      lines (concat (filler line-strings) line-strings)]
  ; (doall (map println lines))
  lines))