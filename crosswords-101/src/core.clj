(ns core)

(defn transpose [m]
  (mapv #(apply str %) (apply mapv vector m)))

(defn x-word [rows]
    {:rows rows :columns (transpose rows)})

(defn str-replace
  "Replace c in string s at index i"
  [s c i]
  (str (subs s 0 i) c (subs s (+ i (count c)))))

(defn re-pos [re s]
  "Clojure does not support regex index results (yet)"
  (loop [m (re-matcher re s)
         res {}]
    (if (.find m)
      (recur m (assoc res (.start m) (.group m)))
      res)))

(defn word->pattern [word]
  "Returns regex pattern using the word
  FOO -> #'([\\w-]*[-F][-O][-O][\\w-]*)'
  We need the extra matches at the start and end, in case
  they are false positives like '+++B---++++',
  which will later be eliminated by a length check"
  (->> word
       (map #(str  "[\\-" % "]"))
       (reduce str)
       (#(str "([\\w-]*" % "[\\w-]*)"))
       re-pattern))

(defn fit-word [line word]
  "Returns a sequence of all possible word / line fills
  Filters out matches that are longer than the word
  word: KEY
  line: +---++---+ -> (+KEY++---+ +---++KEY+)"
  (let [positions (-> word word->pattern (re-pos line))]
    (for [[idx match] positions
          :when (=(count match) (count word))]
      (str-replace line word idx))))

(defn fitted-lines [lines word]
  "Returns all possible permutations of
  placements of the word in all available spaces.
  For each permutation you get a new matrix.

  word KEY
  lines ['+---+FOO++' '+-E-++---+']

  result [['+KEY+FOO++' '+-E-++---+']
          ['+---+FOO++' '+KEY++---+']
          ['+---+FOO++' '+-E-++KEY+']]"
  (let [assoc-word (fn [idx line]
                     (if-let [perms (fit-word line word)]
                       (for [p perms]
                         (assoc lines idx p))))]
    (apply concat (keep-indexed assoc-word lines))))


(defn xw-permutations [{rows :rows columns :columns :as xw} word]
  "Retuns all possible permutations of the crossword with
  the given word fitted in all available spaces"
  (let [row-perms
        (for [rp (fitted-lines rows word)]
          {:rows rp :columns (transpose rp)})
        col-perms
        (for [cp (fitted-lines columns word)]
          {:rows (transpose cp) :columns cp})]
    (concat row-perms col-perms)))

(defn xw-solve [xw words]
  (reduce
   (fn [coll word] (mapcat #(xw-permutations % word) coll)) [xw] words))


(defn solution [input]
    (let [words (clojure.string/split (last input) #";")
          xw (x-word (vec (butlast input)))
          result ((first (xw-solve xw words)) :rows)]
          result))


; Uncomment the lines below to run on hackerrank
; (let [inputs (doall (repeatedly 11 read-line))]
    ; (doall (map println (solution inputs))))
