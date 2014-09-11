(ns kata14-trigrams.core
  (require [clojure.string :as str]
           [clojure.set :refer [union]]))

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
  "Read next sentence from reader 'rdr'"
  ([rdr]
     (read-next-sentence rdr (vector) (vector)))
  ([rdr seen result]
     (let [chr (.read rdr)]
       (if (and chr
                (>= chr 0))
         (let [character (char chr)]
           (if (sentence-end-p character seen)
             result
             (recur rdr (conj seen character)
                    (next-char-result character result))))
         result))))

(defn read-sentences
  "Uses clojure.java.io/reader to read sentences from x."
  [x]
  (letfn [(lfs-helper [rdr]
            (lazy-seq
             (if-let [sentence (read-next-sentence rdr)]
               (cons (apply str sentence) (lfs-helper rdr))
               (do (.close rdr) nil))))]
    (lfs-helper (clojure.java.io/reader x))))

(defn inputs-to-future-ngrams 
  "Convert a sequence of reader-readable inputs (files, urls, etc.) into future ngrams"
  [inputs n]
    (doall
     (map 
      #(future
         (map 
          (fn [tokens] 
            (ngram tokens n))
          (tokenize-sentences (read-sentences %1))))
      inputs)))

(defn ngrams2prefixmap
  "Takes a collection of ngrams and returns a map with prefixes of size 'prefixlength' to remainders."
  [ngrams prefixlength]
  (reduce (fn [m [k v]]
            (assoc m k (union (get m k #{}) (set v))))
          (hash-map)
          (map #(split-at prefixlength %) ngrams)))

(defn collect-mapset
  "Call `f` on collection `coll` which is assumed to return a collection of key value pairs and groups this collection into a map `m` from keys to a set of all values for any given key."
  ([f coll]
     (collect-mapset f coll (hash-map)))
  ([f coll m]
     (reduce (fn [m [k v]]
               (assoc m k (union (get m k #{}) (set v))))
             m
             (f coll))))

(defn ngram-mapset
  [ngrams pl]
  (collect-mapset 
   (fn [coll] 
     (map #(split-at pl %1) coll)) 
   ngrams))