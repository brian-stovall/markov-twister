(ns markov-twister.generator)

(defn word-chain [word-transitions]
  (reduce (fn [accumulator data] (merge-with clojure.set/union accumulator
                                 (let [[a b c] data]
                                 {[a b] (if c #{c} #{})})))
          {}
          word-transitions))

(defn text->word-chain [text]
  (word-chain (partition-all 3 1 (clojure.string/split text #"[\s|\n]"))))

(defn word-count [text]
  (count (clojure.string/split text #"[\s|\n]")))

(defn walk-chain [prefix chain accumulator length]
  (let [suffixes (get chain prefix)]
    (if (or (> (count accumulator) length) (empty? suffixes))
      accumulator
      (let [suffix (first (shuffle suffixes))
            new-prefix [(last prefix) suffix]]
        (recur new-prefix chain (conj accumulator suffix) length)))))

(defn word-chain->story [chain length]
  (let [random-prefix (first (shuffle (keys chain)))]
    (clojure.string/join " " (walk-chain random-prefix chain [] length))))

(defn file->word-chain [filename]
  (text->word-chain (slurp (clojure.java.io/resource filename))))

(defn file->story [filename length]
  (word-chain->story (file->word-chain filename) length))
