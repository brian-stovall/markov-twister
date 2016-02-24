(ns markov-twister.generator)

(defn word-chain [word-transitions]
  (reduce (fn [accumulator data] (merge-with clojure.set/union accumulator
                                 (let [[a b c] data]
                                 {[a b] (if c #{c} #{})})))
          {}
          word-transitions))

(defn text->word-chain [text]
  (word-chain (partition-all 3 1 (clojure.string/split text #"[\s|\n]"))))
