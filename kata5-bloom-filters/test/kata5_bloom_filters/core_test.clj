(ns kata5-bloom-filters.core-test
  (:use clojure.test
        kata5-bloom-filters.core))

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

(deftest bloom-add-returns-an-integer
  (testing "Simple value computation test for a adding a value to a bloom filter"
    (is (= 4755801206511640576 (bloom-add 0 "foobar")))))

(deftest bloom-added-values-are-contained
  (testing "An added value is contained in the resulting bloom filter"
    (is (bloom-contains? (bloom-add 0 "foobar") "foobar"))))