(ns markov-twister.generator)

(defn ending-punctuation? 
  "A predicate that returns a truthy value when char is . ! or ?"
  [char]
  (some #{\. \! \?} char))

(defn capitalized?
  "A predicate that returns true when the first letter of a string is
   capitalized."
  [string]
  (re-matches #"^[A-Z].+" string))

(defn text->word-chain
  "Makes a map from groups of 3 words from 'text'. The first two words of any group comprise 
   the keys. The values are sets, where each set contains each word apprearing after
   that key in the text. This map is known as a word-chain in this program."
  [text]
  (let [grouped-text (partition-all 3 1 (clojure.string/split text #"\s+")) ]
        (reduce (fn [accumulator data] (merge-with clojure.set/union accumulator
                                                   (let [[a b c] data]
                                                     {[a b] (if c #{c} #{})})))
                {}
                grouped-text)))

(defn word-chain->story-list
  "Transforms a word-chain into a list of Markov-generated words by
   starting with a key from the chain and walking it until nil
   is reached or at the first punctuated word after 'length' words
   are generated."
  [prefix chain accumulator length]
  (let [suffixes (get chain prefix)]
    (if (or (and  (> (count accumulator) length) (ending-punctuation? (last accumulator)))
            (empty? suffixes))
      accumulator
      (let [suffix (first (shuffle suffixes))
            new-prefix [(last prefix) suffix]]
        (recur new-prefix chain (conj accumulator suffix) length)))))

(defn word-chain->story
  "Chooses a random prefix and generates a story-list of 'length' words,
   drops words from the front until a capitalized word is found, and then
   joins the words into a string."
  [chain length]
  (let [random-prefix (first (shuffle (keys chain)))
        word-list (word-chain->story-list random-prefix chain [] length)
        sanitized-words (drop-while (complement capitalized?) word-list)]   
     (clojure.string/join " " sanitized-words)))

(defn file->word-chain
  "Makes a word-chain from a text file."
  [filename]
  (text->word-chain (slurp (clojure.java.io/resource filename))))

(defn file->story
  "Makes a story from a file input."
  [filename length]
  (word-chain->story (file->word-chain filename) length))

(defn files->story 
  "Makes and unifies the word-chains for an arbitraty number of input files, and then
  builds a story from that word-chain. "
  [length & files]
  (let [chains (map file->word-chain files)]
    (word-chain->story (apply merge-with clojure.set/union chains) length)))
