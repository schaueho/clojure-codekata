(ns kata14-trigrams.core
  (require [clojure.string :as str]))

(defn ngram
  "Given a sequence sq and a number n, returns a sequence of new contiguous sequences
 of n items that appear in sq."
  ([squence n]
     (lazy-seq
      (ngram squence n [])))
  ([squence n aux]
     (if-let [sq (seq squence)]
       (recur (rest sq) n (conj aux (take n sq)))
       aux)))

(defn split-on-whitespace
  "Take a string and split it's content on whitespace, removing the whitespace"
  [string]
  (str/split string #"\s+"))

(defn split-off-punctuation
  "Take a string and split it's content, keeping punctuation as new tokens"
  [string]
  (let [match (re-find #"(\w+)(\p{Punct})?" string)
        result (rest (keep identity match))]
    (if (seq result)
      result
      (vector ""))))

(defn tokenize-sentence
  "Take a single sentence and return a sequence of tokens for it"
  [sentence]
  (flatten (map split-off-punctuation
                (split-on-whitespace sentence))))

(defn tokenize-sentences
  "Take a sequence of sentences and return a sequence of tokens for  each sentence"
  [sentences]
  (map tokenize-sentence sentences))

(defn split-sentences
  "Split a string into a sequence of sentences"
  [string]
  (str/split string #"\s*[.!?](\s+|$)"))

(defn tokenize
  "Tokenize a string"
  [string]
  (-> string
      (split-sentences)
      (tokenize-sentences)))

(defn sentence-end-p [character charstack]
  (cond (and (= character \space)
             (some (partial = (peek charstack)) [\. \? \!])) true
        (and (= character \return)
             (some (partial = (peek charstack)) [\. \? \!])) true
        (and (= character \newline)
             (some (partial = (peek charstack)) [\. \? \!])) true
        (and (= character \newline)
             (or (= (peek charstack) \newline)
                   (and (= (peek charstack) \return)
                        (= (peek (pop charstack)) \newline)))) true
          :else false))

(defn next-char-result [character charstack]
  (cond (and (empty? charstack)
             (or (= character \space)
                 (= character \tab)
                 (= character \newline)
                 (= character \return))) charstack
        (and (= character \space)
             (= (peek charstack) \space))  charstack
        (and (= character \tab)
             (= (peek charstack) \space))  charstack
        (and (= character \tab)
             (= (peek charstack) \tab))  (conj (pop charstack) \space) ; should never happen
        (= character \tab) (conj charstack \space)
        (= character \return) charstack
        (and (= character \newline)
             (= (peek charstack) \space)) charstack
        (and (= character \newline)
             (= (peek charstack) \return)) (conj (pop charstack) \space) ; should never happen
        (= character \newline) (conj charstack \space)
        :else (conj charstack character)))

(defn read-next-sentence
  ([rdr]
     (read-next-sentence rdr (vector) (vector)))
  ([rdr seen result]
     (if-let [chr (.read rdr)]
       (when (>= chr 0)
         (let [character (char chr)]
           (if (sentence-end-p character seen)
             result
             (recur rdr (conj seen character)
                    (next-char-result character result)))))
       result)))

(defn file-sentences [file]
  (letfn [(lfs-helper [rdr]
            (lazy-seq
             (if-let [sentence (seq (read-next-sentence rdr))]
               (cons (apply str sentence) (lfs-helper rdr))
               (do (.close rdr) nil))))]
    (lfs-helper (clojure.java.io/reader file))))