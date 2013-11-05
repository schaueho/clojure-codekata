(ns kata6-anagrams.core-test
  (:use clojure.test
        kata6-anagrams.core))

(deftest setimpl-succeeds-for-real-anagrams
  (testing "anagram-set"
    (is (= true (anagram-set "the law" "wealth")))))

(deftest setimpl-succeeds-for-nonreal-anagrams
  (testing "anagram-set is too simplistic"
    (is (= true (anagram-set "the lalalaw" "wealth")))))

(deftest hashmap-succeeds-for-real-anagrams
  (testing "anagram-hash-map implementation"
    (is (= true (anagram-phm "the law" "wealth")))))

(deftest hashmap-fails-for-nonreal-anagrams
  (testing "anagram-hash-map is too simplistic"
    (is (= false (anagram-phm "the lalalaw" "wealth")))))
