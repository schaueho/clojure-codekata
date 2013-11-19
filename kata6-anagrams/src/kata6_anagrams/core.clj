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

(defn find-largest-index-with-bigger-successor [squence]
  (loop [restsq (seq squence)
         curpos 0
         curresult nil]
    (cond (or (empty? restsq)
              (empty? (rest restsq)))
          curresult
          ((comp pos? compare) (second restsq) (first restsq))
          (recur (rest restsq)
                 (inc curpos)
                 curpos)
          :else
          (recur (rest restsq)
                 (inc curpos)
                 curresult))))


(defn find-largest-index-bigger-value [squence index]
  (let [compval (nth (vec squence) index)]
    (loop [restsq (seq squence)
           curpos 0
           curresult nil]
    (cond (empty? restsq)
          curresult
          ((comp pos? compare) (first restsq) compval)
          (recur (rest restsq)
                 (inc curpos)
                 curpos)
          :else
          (recur (rest restsq)
                 (inc curpos)
                 curresult)))))

(defn swap-positions [squence k l]
  (let [seqvec (vec squence)]
    (assoc (assoc seqvec k (nth seqvec l))
      l (nth seqvec k))))

(defn reverse-tail [squence tail-position]
  (let [prefix (take tail-position squence)
        tail (drop tail-position squence)
        revtail (reverse tail)]
    (concat prefix revtail)))

(defn next-permutation [squence]
  (when-let [k (find-largest-index-with-bigger-successor squence)]
    (let [l (find-largest-index-bigger-value squence k)
          swapped (swap-positions squence k l)
          current-perm (reverse-tail swapped (inc k))]
      current-perm)))

(defn generate-permutations-eager [squence]
  ;; generate permutations in lexicographic order, following Naranya Pandita, 14th century
  ;; cf. https://en.wikipedia.org/wiki/Permutation
  (let [start-perm (sort squence)]
    (loop [permutation (next-permutation start-perm)
           result (list start-perm)]
      (if (or (not permutation)
              (empty? permutation))
        result
        (recur (next-permutation permutation)
               (concat result (list permutation)))))))

(defn- gen-perms [squenze]
  (lazy-seq
   (when-let [permutation (next-permutation squenze)]
     (cons permutation (gen-perms permutation)))))

(defn generate-permutations [squence]
  (let [start-perm (sort squence)]
        (cons start-perm (gen-perms start-perm))))

(defn generate-anagrams [word]
  "Generate all anagrams of word"
  (map (partial apply str) (generate-permutations word)))

(defn find-anagrams [word words]
  "Finds all anagrams of word in (the sequence of) words"
  (let [anagrams (generate-anagrams word)
        wordset (set words)]
    (loop [candidates anagrams
           result []]
      (if (empty? candidates)
        result
        (recur (rest candidates)
               (if (and (not (= (first candidates) word))
                        (contains? wordset (first candidates)))
                 (concat result (list (first candidates)))
                 result))))))
