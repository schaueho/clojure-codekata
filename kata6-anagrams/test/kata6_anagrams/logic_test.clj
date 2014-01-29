(ns kata6-anagrams.logic-test
  (:refer-clojure :exclude [==])
  (:use [midje.sweet]
        [clojure.core.logic]
        [kata6-anagrams.logic])
  (:require [clojure.string :refer [split-lines]]))

(facts "Testing the set implementation for checking anagrams"
       (fact "The same word is an anagram of itself"
             (anagram? "the law" "the law") => (just "the law"))
       (fact "anagram can find anagrams"
             (anagram? "the law" "wealth") => true))

(facts "Checking insert"
       (fact "Simple insert"
             (run* [q] (insert 1 [2] q)) => '((1 2))
             (run* [q] (insert 1 [2 3] q)) => '((1 2 3)))
       (fact "We can retrieve the inserted data"
             (run* [q] (insert q [2] '(1 2))) => '(1))
       (fact "We can retrieve the list data, too"
             (run* [q] (insert 1 q '(1 2))) => '((2))))
