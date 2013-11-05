(ns kata6-anagrams.core-test
  (:use clojure.test
        midje.sweet
        kata6-anagrams.core))

(facts "Testing the set implementation for checking anagrams"
       (fact "Set anagram can find anagrams"
             (anagram-set? "the law" "wealth") => true)
       
       (fact "Set anagram is too simplistic"
             (anagram-set? "the lalalaw" "wealth") => true))

(facts "Testing the set implementation for checking anagrams"
       (fact "anagram-hash-map implementation"
             (anagram-phm? "the law" "wealth") => true)

       (fact "anagram-hash-map fails on non-anagrams"
             (anagram-phm? "the lalalaw" "wealth") => false))
