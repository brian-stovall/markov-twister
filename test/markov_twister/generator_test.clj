(ns markov-twister.generator-test
  (:require [clojure.test :refer :all]
            [clojure.set]
            [markov-twister.generator :refer :all]
            ))

(deftest test-word-chain
  (testing "it produces a chain of the possible two step transitions between suffixes and prefixes"
    (let [example '(("And" "the" "Golden")
                    ("the" "Golden" "Grouse")
                    ("And" "the" "Pobble")
                    ("the" "Pobble" "who"))]
      (is (= {["the" "Pobble"] #{"who"}
              ["the" "Golden"] #{"Grouse"}
              ["And" "the"] #{"Pobble" "Golden"}}
             (word-chain example))))))

(deftest test-text->word-chain
  (testing "string with spaces and newlines"
    (let [example "And the Golden Grouse\nAnd the Pobble who"]
     (is (= {["who" nil] #{}
             ["Pobble" "who"] #{}
             ["the" "Pobble"] #{"who"}
             ["Grouse" "And"] #{"the"}
             ["Golden" "Grouse"] #{"And"}
             ["the" "Golden"] #{"Grouse"}
             ["And" "the"] #{"Pobble" "Golden"}}
            (text->word-chain example))))))

(deftest test-walk-chain
  (let [chain {["who" nil] #{},
               ["Pobble" "who"] #{},
               ["the" "Pobble"] #{"who"},
               ["Grouse" "And"] #{"the"},
               ["Golden" "Grouse"] #{"And"},
               ["the" "Golden"] #{"Grouse"},
               ["And" "the"] #{"Pobble" "Golden"}}]
    (testing "dead end"
      (let [prefix ["the" "Pobble"]]
        (is (= ["the" "Pobble" "who"]
               (walk-chain prefix chain prefix)))))))
