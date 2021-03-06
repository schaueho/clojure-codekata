(ns kata6-anagrams.logic-test
  (:refer-clojure :exclude [==])
  (:use [midje.sweet]
        [clojure.core.logic]
        [clojure.tools.trace]
        [kata6-anagrams.logic])
  (:require [clojure.string :refer [split-lines]]))

(facts "Checking insert"
       (fact "Simple insert"
             (run* [q] (insert 1 [2] q)) => '((1 2) (2 1))
             (run* [q] (insert 1 [2 3] q)) => '((1 2 3) (2 1 3) (2 3 1)))
       (fact "We can retrieve the inserted data"
             (run* [q] (insert q [2] '(1 2))) => '(1))
       (fact "We can retrieve the list data, too"
             (run* [q] (insert 1 q '(1 2))) => '((2))))

(facts "Checking inserto"
       (fact "Simple insert"
             (run* [q] (inserto [2] 1 q)) => '((1 2) (2 1))
             (run* [q] (inserto [2 3] 1 q)) => '((1 2 3) (2 1 3) (2 3 1)))
       (fact "We can retrieve the inserted data"
             (run* [q] (inserto [2] q '(1 2))) => '(1))
       (fact "We can retrieve the list data, too"
             (run* [q] (inserto q 1 '(1 2))) => '((2))))

;(trace-vars kata6-anagrams.logic/permuto)
(facts "Checking permuto2"
       (fact "An empty permutation"
             (run* [q] (permuto nil nil)) => '(_0)
             (run* [q] (permuto '() ())) => '(_0))
       (fact "A simple permutation"
             (run* [q] (permuto '(1) q)) => '((1)))
       (fact "A more complex permutation"
             (run* [q] (permuto '(1 2) q)) => '((1 2) (2 1)))
       (fact "An even more complex permutation"
             (run* [q] (permuto '(1 2 3) q)) => '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))))

(facts "Checking permuto3"
       (fact "An empty permutation"
             (run* [q] (permuto3 nil nil nil)) => '(_0)
             (run* [q] (permuto3 '() () ())) => '(_0))
       (fact "A simple permutation"
             (run* [q] (permuto3 '(1) q q)) => '((1)))
       (fact "A more complex permutation"
             (run* [q] (permuto3 '(1 2) q q)) => '((1 2) (2 1)))
       (fact "An even more complex permutation"
             (run* [q] (permuto3 '(1 2 3) q q)) => '((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1)))
       (fact "A simple charlist?"
             (run* [q] (permuto3 (seq "al") q q)) => '((\a \l) (\l \a))))

(facts "Checking permutation"
       (fact "An empty permutation"
             (run* [q] (permutation nil nil)) => '(_0)
             (run* [q] (permutation '() ())) => '(_0))
       (fact "A simple permutation"
             (run* [q] (permutation '(1) q)) => '((1)))
       (fact "A more complex permutation"
             (run* [q] (permutation '(1 2) q q)) => '((1 2) (2 1)))
       (fact "An even more complex permutation"
             (run* [q] (permutation '(1 2 3) q)) => '((1 2 3) (2 1 3) (2 3 1) (1 3 2) (3 1 2) (3 2 1))))

;(trace-vars kata6-anagrams.logic/permuto3)
(facts "Testing the anagram implementation"
       (fact "Generating all anagrams"
             (generate-anagrams "ftw") => '("ftw" "fwt" "tfw" "twf" "wft" "wtf"))
       (let [words (split-lines (slurp "wordlist.txt"))]
             (find-anagrams "kinship" words) => '("pinkish" "pinkish")
             (find-anagrams "enlist" words) => '("listen" "inlets" "silent")
             (find-anagrams "boaster" words) => '("boaters" "borates")
             (find-anagrams "sinks" words) => '("skins" "skins")
             (find-anagrams "knits" words) => '("stink")
             (find-anagrams "rots" words) => '("sort")
             (find-anagrams "thelaw" words) => '("wealth")))
