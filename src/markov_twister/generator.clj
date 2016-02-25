(ns markov-twister.generator)

(defn ending-punctuation? 
  "A predicate that returns a truthy value when char is . ! or ?"
  [string]
  (if (char? string)
    false
    (re-matches #".*[.?!]$" string)))

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

(defn format-word-list
  "Adds tab and newline characters to a list of words to make lines less than 'line-length'
  characters long and broken into paragraphs."
  [word-list]
  (let [line-length 70
        paragraph-lengths (range 3 9)]
    (loop [accumulator [\tab]
           character-count 0
           line-count 0
           words-left word-list]
      (let [current-word (first words-left)]
        (cond (empty? words-left)
              accumulator
              (> (+ character-count (count current-word)) line-length)
              (recur (conj accumulator \newline current-word)
                     (count current-word)
                     (inc line-count)
                     (rest words-left))
              (and (ending-punctuation? current-word) (> line-count (rand-nth paragraph-lengths)))
              (recur (conj accumulator current-word \newline \newline \tab)
                     (count current-word)
                     0
                     (rest words-left))
              :else
              (recur (conj accumulator current-word)
                     (+ character-count (count current-word))
                     line-count
                     (rest words-left)))))))

(defn word-chain->story
  "Chooses a random prefix and generates a story-list of 'length' words,
   drops words from the front until a capitalized word is found, and then
   joins the words into a nicely-formatted string."
  [chain length]
  (let [random-prefix (first (shuffle (keys chain)))
        word-list (word-chain->story-list random-prefix chain [] length)
        formatted-list (format-word-list (drop-while (complement capitalized?) word-list))]   
    (clojure.string/join " " formatted-list)))

(defn file->word-chain
  "Makes a word-chain from a text file."
  [filename]
  (text->word-chain (slurp (clojure.java.io/resource filename))))

(defn files->word-chain
  "Unifies the word-chains from many files into one."
  [& files]
  (let [chains (map file->word-chain files)]
    (apply merge-with clojure.set/union chains)))

(defn file->story
  "Makes a story from a single file input."
  [filename length]
  (word-chain->story (file->word-chain filename) length))

(defn files->story 
  "Makes and unifies the word-chains for an arbitraty number of input files, and then
  builds a story from that word-chain. "
  [length & files]
  (word-chain->story (apply files->word-chain files) length))
