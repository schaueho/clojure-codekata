(ns kata5-bloom-filters.core-test
  (:require [clojure.string :as string])
  (:use clojure.test
        kata5-bloom-filters.core)
  (:import (java.util BitSet)
           (java.util.concurrent Executors)))

(deftest sum-of-chars-returns-valid-result
  (testing "Testing hash function sum-chars"
    (is (= 633 (sum-chars "foobar")))))

(deftest djb-string-hash-returns-valid-result
  (testing "Testing hash function djb-string-match"
    (is (= 6953516687550 (djb-string-hash "foobar")))))

(deftest sdbm-string-hash-returns-valid-result
  (testing "Testing hash function sdbm-string-hash"
    (is (= (sdbm-hash-recur "foobar") (sdbm-string-hash "foobar")))))

(deftest fnvhash-hash-returns-valid-result
  (testing "Testing hash function fnv-hash"
    (is (= 6157838686567520599 (fnv-hash "foobar")))))

(deftest optimal-size-calculates-value
  (testing "Optimal size calculation for bloom filter"
    (is (= 313751.0 (optimal-size 45420 1/1000)))))


(deftest bloom-added-values-are-contained
  (testing "An added value is contained in the resulting bloom filter"
    (is (bloom-contains? (bloom-add (BitSet. 100000) "foobar") "foobar"))))

(deftest bloom-of-dict-words-finds-data
  (testing "A fully build bloom filter finds the data that got added"
    (let [bloom (build-bloom "wordlist.txt" :size 313751)]
      (is (bloom-contains? bloom "aback"))
      (is (bloom-contains? bloom "abandons"))
      (is (not (bloom-contains? bloom "foo"))))))

(defn make-random-string []
  (let [numchars (rand-nth (range 1 8))
        characters (map char (take numchars (repeatedly #(rand-nth (range 97 122)))))]
    (apply str characters)))

(defn bloom-add-random-string [bloom-filter]
  (let [newstring (make-random-string)]
    (println newstring)
    (bloom-add bloom-filter newstring)))

(deftest bloom-bit-vector-follows-protocol
  (let [bloom (make-bloom-vector 10)]
    (is (bloom-size bloom) 10)
    (dosync
     (bloom-bit-set bloom 3 true)
     (bloom-bit-set bloom 4 true))
    (is (= (bloom-bit-get bloom 3) true))
    (is (= (bloom-bit-get bloom 4) true))
    (is (= (bloom-bit-get bloom 7) false))))

(deftest bloom-bit-vol-follows-protocol
  (let [bloom (make-bloom-vol 100)]
    (is (bloom-size bloom) 10)
    (dosync
     (bloom-bit-set bloom 3 true)
     (bloom-bit-set bloom 4 true))
    (is (= (bloom-bit-get bloom 3) true))
    (is (= (bloom-bit-get bloom 4) true))
    (is (= (bloom-bit-get bloom 7) false))))

(deftest synced-bloom-of-dict-words-finds-data
  (testing "A fully build bloom filter finds the data that got added"
    (let [bloom (build-bloom-synced "wordlist.txt" :bloom-filter (make-bloom-vector 313751))]
      (is (bloom-contains? bloom "aback"))
      (is (bloom-contains? bloom "abandons"))
      (is (not (bloom-contains? bloom "foo"))))))


; Fogus etal. dothreads helper
(def ^:dynamic *pool*
  (Executors/newFixedThreadPool
   (+ 2 (.availableProcessors (Runtime/getRuntime)))))

(defn dothreads! [f & {thread-count :threads exec-count :times
                       :or {thread-count 1 exec-count 1}}]
  (dotimes [t thread-count]
    (.submit *pool* #(dotimes [_ exec-count] (f)))))

(defn make-random-boolean-array [size]
  (boolean-array
   (take size (repeatedly #(rand-nth [true false])))))

(defn make-random-boolean-vector [size]
  (into (vector-of :boolean) (take size (repeatedly #(rand-nth [true false])))))

(defn print-boolean-vector-seq [bv]
  (loop [truthvals bv
         result ""]
    (if (empty? truthvals)
      result
      (recur (rest truthvals) (string/join [result (if (first truthvals) 1 0)])))))

(defn print-boolean-array [ba]
  (let [size (count ba)]
    (loop [indx 0
           result ""]
    (if (= indx size)
      result
      (recur (inc indx) (string/join [result (if (aget ba indx) 1 0)]))))))

(defn print-boolean-vector [bv]
  (let [size (count bv)]
    (loop [indx 0
           result ""]
    (if (= indx size)
      result
      (recur (inc indx) (string/join [result (if (get bv indx) 1 0)]))))))

(defn print-flipped-boolean-array [ba]
  (let [size (count ba)]
    (loop [indx 0
           result ""]
    (if (= indx size)
      result
      (do
        (aset ba indx (not (aget ba indx)))
        (recur (inc indx)
               (string/join
                [result
                 (if (aget ba indx) 1 0)])))))))

(defn print-flipped-boolean-vector [bv]
  (let [size (count bv)]
    (loop [indx 0
           vec bv
           result ""]
    (if (= indx size)
      result
      (let [newvec (assoc vec indx (not (get vec indx)))]
      (recur (inc indx)
             newvec
             (string/join
              [result
               (if (get newvec indx) 1 0)])))))))
