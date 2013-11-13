(ns kata6-anagrams.core-test
  (:require [clojure.string :refer [split-lines]])
  (:use midje.sweet
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

(facts "Testing the implementation details of the Pandita algorithm"
       (fact "finding the largest index with bigger successor"
             (find-largest-index-with-bigger-successor [1 2]) => 0
             (find-largest-index-with-bigger-successor [1 2 3 4]) => 2
             (find-largest-index-with-bigger-successor [1 2 4 3]) => 1
             (find-largest-index-with-bigger-successor [1 3 4 2]) => 1
             (find-largest-index-with-bigger-successor [1 4 3 2]) => 0)
       (fact "finding the largest index that has a bigger value than some other position"
             (find-largest-index-bigger-value [1 2 3 4] 2) => 3
             (find-largest-index-bigger-value [1 2 4 3] 1) => 3
             (find-largest-index-bigger-value [1 3 4 2] 1) => 2
             (find-largest-index-bigger-value [1 4 3 2] 0) => 3)
       (fact "swapping two positions in a sequence"
             (swap-positions [1 2 3 4] 2 3) => [1 2 4 3]
             (swap-positions [1 2 4 3] 1 3) => [1 3 4 2]
             (swap-positions [1 3 4 2] 1 2) => [1 4 3 2]
             (swap-positions [1 4 3 2] 0 3) => [2 4 3 1])
       (fact "reverse the tail of a sequence"
             (reverse-tail [1 2 4 3] 2) => [1 2 3 4]
             (reverse-tail [1 3 4 2] 1) => [1 2 4 3]
             (reverse-tail [1 4 3 2] 0) => [2 3 4 1])
       (fact "finding the next permutation"
             (next-permutation [1 2 3 4]) => [1 2 4 3]
             (next-permutation [1 2 4 3]) => [1 3 2 4])
       (fact "generating all permutations"
             (generate-permutations [1 2 3 4]) => '((1 2 3 4) (1 2 4 3)
                                                    (1 3 2 4) (1 3 4 2)
                                                    (1 4 2 3) (1 4 3 2)
                                                    (2 1 3 4) (2 1 4 3)
                                                    (2 3 1 4) (2 3 4 1)
                                                    (2 4 1 3) (2 4 3 1)
                                                    (3 1 2 4) (3 1 4 2)
                                                    (3 2 1 4) (3 2 4 1)
                                                    (3 4 1 2) (3 4 2 1)
                                                    (4 1 2 3) (4 1 3 2)
                                                    (4 2 1 3) (4 2 3 1)
                                                    (4 3 1 2) (4 3 2 1))
             (count (generate-permutations [1 2 3 4])) => 24))

(facts "Testing the anagram implementation"
       (fact "Generating all anagrams"
             (generate-anagrams "ftw") => '("ftw" "fwt" "tfw" "twf" "wft" "wtf"))
       (let [words (split-lines (slurp "wordlist.txt"))]
             (find-anagrams "kinship" words) => '("pinkish")
             (find-anagrams "enlist" words) => '("inlets" "listen" "silent")
             (find-anagrams "boaster" words) => '("boaters" "borates")
             (find-anagrams "sinks" words) => '("skins")
             (find-anagrams "knits" words) => '("stink")
             (find-anagrams "rots" words) => '("sort")
             (find-anagrams "thelaw" words) => '("wealth")))
