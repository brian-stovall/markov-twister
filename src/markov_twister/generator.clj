(ns markov-twister.generator)

(defn word-chain [word-transitions]
  (reduce (fn [accumulator data] (merge-with clojure.set/union accumulator
                                 (let [[a b c] data]
                                 {[a b] (if c #{c} #{})})))
          {}
          word-transitions))

(defn text->word-chain [text]
  (word-chain (partition-all 3 1 (clojure.string/split text #"[\s|\n]"))))

(defn walk-chain [prefix chain accumulator]
  (let [suffixes (get chain prefix)]
    (if (empty? suffixes)
      accumulator
      (let [suffix (first (shuffle suffixes))
            new-prefix [(last prefix) suffix]]
        (recur new-prefix chain (conj accumulator suffix))))))

(defn text->story [text]
  (let [chain (text->word-chain text)
        random-prefix (first (shuffle (keys chain)))]
    (clojure.string/join " " (walk-chain random-prefix chain []))))
