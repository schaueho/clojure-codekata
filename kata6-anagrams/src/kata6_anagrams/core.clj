(ns kata6-anagrams.core
  (require [clojure.string :as str]))

(defn remove-blanks [word]
  (str/replace word " " ""))

(defn anagram-set? [word1 word2]
  (let [w1 (remove-blanks word1)
        w2 (remove-blanks word2)] 
    (= (set w1) (set w2))))

(defn anagram-phm? [word1 word2]
  (let [add-char-to-charmap
        (fn [charmap char]
          (assoc charmap char
                 (if-let [charoccurences (get charmap char)]
                   (inc charoccurences)
                   1)))
        word-to-charmap 
        (fn [word]
          (reduce add-char-to-charmap
                  (hash-map) 
                  (str/split word (re-pattern ""))))
        chars1 (word-to-charmap (remove-blanks word1))
        chars2 (word-to-charmap (remove-blanks word2))]
    (= chars1 chars2)))



