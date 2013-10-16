(ns kata5-bloom-filters.core-test
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

; Fogus etal. dothreads helper
(def ^:dynamic *pool*
  (Executors/newFixedThreadPool
   (+ 2 (.availableProcessors (Runtime/getRuntime)))))

(defn dothreads! [f & {thread-count :threads exec-count :times
                       :or {thread-count 1 exec-count 1}}]
  (dotimes [t thread-count]
    (.submit *pool* #(dotimes [_ exec-count] (f)))))

