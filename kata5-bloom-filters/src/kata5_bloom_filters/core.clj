(ns kata5-bloom-filters.core
  (:require [kata5-bloom-filters.hashes :refer (sum-chars djb-string-hash sdbm-hash-recur fnv-hash)]
            [clojure.math.numeric-tower :as math :refer (abs ceil floor)]
            [clojure.string :as string])
  (:import (java.util BitSet)))


(def ^:dynamic *hash-functions*
  (list sum-chars djb-string-hash 
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
                   (take (int (ceil (abs (float (/ size 64)))))
                         (repeatedly #(identity 0)))))
        vec-pos (fn [position] 
                  (int (floor (abs (float (/ position 64))))))
        bit-pos (fn [position] 
                  (abs (mod position 64)))]
    (reify BloomFilterImpl
      (bloom-size [_]
        size)
      (bloom-bit-get [_ position]
         (bit-test (nth @bloom-vect (vec-pos position))
                   (bit-pos position)))
      (bloom-bit-set [_ position value]
        (let [oldval (nth @bloom-vect (vec-pos position))]
          (alter bloom-vect assoc (vec-pos position)
                 (bloom-bit-set oldval (bit-pos position) 
                                (if value 0 1))))))))

(defn bloom-add [bloom charseq & {:keys [hashfns] :or {hashfns *hash-functions*}}]
  (let [size (bloom-size bloom)]
    (doseq [hashval (hash-string charseq :hashfns hashfns)]
      (bloom-bit-set bloom (abs (mod hashval size)) true))
    bloom))

(defn bloom-contains? [bloom charseq & {:keys [hashfns] :or {hashfns *hash-functions*}}]
  (let [size (bloom-size bloom)
        hashvals (hash-string charseq :hashfns hashfns)]
    (every? #(= (bloom-bit-get bloom (abs (mod % size))) true)
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
