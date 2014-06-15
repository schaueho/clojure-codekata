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
