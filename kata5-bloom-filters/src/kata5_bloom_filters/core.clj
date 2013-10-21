(ns kata5-bloom-filters.core
  (:require [clojure.string :as string]
            [clojure.math.numeric-tower :as math])
  (:import (java.util BitSet)))


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

(defprotocol BloomFilterImpl
  (bloom-size [filter])
  (bloom-bit-get [filter position])
  (bloom-bit-set [filter position value]))

(extend-type Long
  BloomFilterImpl
  (bloom-size [filter]
    64)
  (bloom-bit-get [filter position]
    (bit-test filter position))
  (bloom-bit-set [filter position value]
    (cond (and value (bit-test filter position))
          filter
          (and value (not (bit-test filter position)))
          (bit-flip filter position)
          (and (not value) (not (bit-test filter position)))
          filter
          (and (not value) (bit-test filter position))
          (bit-flip filter position))))

(extend-type BitSet
  BloomFilterImpl
  (bloom-size [filter]
    (.size filter))
  (bloom-bit-get [filter position]
    (locking filter
      (.get filter position)))
  (bloom-bit-set [filter position value]
    (if (< position (bloom-size filter))
      (locking filter
        (.set filter position value))
      (throw (IllegalArgumentException. "position outside of bloom filter size")))))

(defn make-bloom-vector [size]
  (let [bloom-vect
        (ref (into (vector-of :boolean)
                   (take size (repeatedly #(identity false)))))]
    (reify BloomFilterImpl
      (bloom-size [_]
        size)
      (bloom-bit-get [_ position]
        (nth @bloom-vect position))
      (bloom-bit-set [_ position value]
        (alter bloom-vect assoc position value)))))

(defn make-bloom-vol [size]
  (let [bloom-vect
        (ref (into (vector-of :long)
                   (take (int (Math/ceil (Math/abs (float (/ size 64)))))
                         (repeatedly #(identity 0)))))]
    (reify BloomFilterImpl
      (bloom-size [_]
        size)
      (bloom-bit-get [_ position]
         (bit-test (nth @bloom-vect (int (Math/floor (Math/abs (float (/ position 64))))))
                   (Math/abs (mod position 64))))
      (bloom-bit-set [_ position value]
        (let [oldval (nth @bloom-vect (int (Math/floor (Math/abs (float (/ position 64))))))]
          (alter bloom-vect assoc (int (Math/floor (Math/abs (float (/ position 64)))))
                 (bloom-bit-set oldval (Math/abs (mod position 64)) 
                                (if value 0 1))))))))

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

(defn build-bloom-synced [wordfile & {:keys [bloom-filter size hashfns]
                               :or {size 1024
                                    hashfns *hash-functions*}}]
  (let [bloom (or bloom-filter (BitSet. size))]
        (reduce #(dosync (bloom-add %1 %2 :hashfns hashfns))
                (cons bloom (string/split-lines (slurp wordfile))))
    bloom))

(defn optimal-size [capacity fault-rate]
  (math/ceil (* (Math/log (/ 1 fault-rate)) (Math/log (math/expt Math/E 1)) capacity)))

"let flowers bloom!"
