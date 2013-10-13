(ns kata5-bloom-filters.core
  (:require [clojure.string :as string]
            [clojure.math.numeric-tower :as math])
  (:import (java.util BitSet)
           (java.util.concurrent Executors)))

; cf. http://www.cse.yorku.ca/~oz/hash.html
(defn sum-chars 
  "Sum up the chars of a given string"
  ; this is essentially K&R "lose-lose" pretty bad hashing algorithm
  [charseq]
  (reduce + (map int charseq)))

(defn djb-string-hash
  "Use djb's method for hashing a string"
  [charseq]
  (reduce (fn [curhash charval]
            ; (println (string/join " " [curhash charval]))
            ^long (unchecked-add 
                   (unchecked-add (bit-shift-left curhash 5) curhash) 
                   charval))
          (cons 5381 (map int charseq))))

(defn- comp-hash-val
  [curhash charval]
  (unchecked-subtract 
   ^long (unchecked-add
          charval
          (unchecked-add (bit-shift-left curhash 16) (bit-shift-left curhash 6)))
   (unchecked-long curhash)))

(defn sdbm-string-hash
  "Use the method from sdbm for hashing a string"
  [charseq]
  (reduce comp-hash-val
          (cons  0 (map int charseq))))

(defn sdbm-hash-recur 
  "Recursive version of the sdbm hash"
  ;; Compute hash(i) = hash(i - 1) * 65599 + str[i]; 
  ([charseq] (sdbm-hash-recur charseq 0))
  ([charseq ^long acc]
     (if (empty? charseq)
       acc
       (recur (rest charseq) 
              (unchecked-add (unchecked-multiply acc 65599) 
                             (int (first charseq)))))))
  
; cf. http://www.isthe.com/chongo/tech/comp/fnv/
(defn fnv-hash [charseq]
  (let [fnv-prime 0x811C9DC5]
    (reduce (fn [^long curhash charval]
              ^long (bit-xor (unchecked-multiply curhash fnv-prime) charval))
            (cons 0 (map (comp unchecked-long int) charseq)))))

(def ^:dynamic *hash-functions*
  (list sum-chars ;djb-string-hash 
        sdbm-hash-recur fnv-hash))

(defn hash-string [charseq & {:keys [hashfns] :or {hashfns *hash-functions*}}]
  (map #(% charseq) hashfns))

(defmulti bloom-size type)
(defmethod bloom-size BitSet [bitset]
  (.size bitset))
(defmethod bloom-size Long [bitset]
  64)

(defmulti bloom-bit-get
  (fn [bloomfilter position]
    (type bloomfilter)))
(defmethod bloom-bit-get BitSet [bitset position]
  (locking bitset
    (.get bitset position)))
(defmethod bloom-bit-get Long [bitset position]
  (bit-test bitset position))

(defmulti bloom-bit-set
  (fn [bloomfilter position value]
    (type bloomfilter)))
(defmethod bloom-bit-set BitSet [bitset position value]
  (locking bitset
    (.set bitset position value)))
(defmethod bloom-bit-set Long [bitset position value]
  (cond (and value (bit-test bitset position))
        bitset
        (and value (not (bit-test bitset position)))
        (bit-flip bitset position)
        (and (not value) (not (bit-test bitset position)))
        bitset
        (and (not value) (bit-test bitset position))
         (bit-flip bitset position)))


(defn bloom-add [bloom charseq & {:keys [hashfns] :or {hashfns *hash-functions*}}]
  (let [size (bloom-size bloom)]
    (doseq [hashval (hash-string charseq :hashfns hashfns)]
      (bloom-bit-set bloom (Math/abs (mod hashval size)) true))
    bloom))

(defn bloom-contains? [bloom charseq & {:keys [hashfns] :or {hashfns *hash-functions*}}]
  (let [size (bloom-size bloom)
        hashvals (hash-string charseq :hashfns hashfns)]
    (every? #(= (bloom-bit-get bloom (Math/abs (mod % size))) true)
            hashvals)))

(defn build-bloom [wordfile & {:keys [bloom-filter size hashfns]
                               :or {size 1024
                                    hashfns *hash-functions*}}]
  (let [bloom (or bloom-filter (BitSet. size))]
        (reduce #(bloom-add %1 %2 :hashfns hashfns)
                (cons bloom (string/split-lines (slurp wordfile))))
    bloom))

(def *pool* (Executors/newFixedThreadPool
             (+ 2 (.availableProcessors (Runtime/getRuntime)))))
(defn dothreads! [f & {thread-count :threads exec-count :times
                       :or {thread-count 1 exec-count 1}}]
  (dotimes [t thread-count]
    (.submit *pool* #(dotimes [_ exec-count] (f)))))

(defn optimal-size [capacity fault-rate]
  (math/ceil (* (Math/log (/ 1 fault-rate)) (Math/log (math/expt Math/E 1)) capacity)))
