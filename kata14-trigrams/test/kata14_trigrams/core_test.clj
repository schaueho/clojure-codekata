(ns kata14-trigrams.core-test
  (:use midje.sweet
        kata14-trigrams.core))

(fact "n-grams explodes a sequence into sequence of sequences of length n"
      (ngram [1 2 3 4 5 6] 3) => ['(1 2 3) '(2 3 4) '(3 4 5)
                                  '(4 5 6) '(5 6) '(6)])

(fact "Splitting but keeping punctuation if any"
      (split-off-punctuation "ready,") => ["ready" ","]
      (split-off-punctuation "ready") => ["ready"]
      (split-off-punctuation "!+#?") => [""])

(fact "Splitting on whitespace"
      (split-on-whitespace "Are you  ready") => ["Are" "you" "ready"]
      (split-on-whitespace "Are") => ["Are"])

(fact "Tokenize a sentence"
      (tokenize-sentence "Are you ready, Tom?") => '("Are" "you" "ready" "," "Tom" "?"))

(fact "Split sentences in a string"
      (split-sentences "Are you ready, Tom? I want to go.") => ["Are you ready, Tom" "I want to go"])

(fact "Tokenize some sentences"
      (tokenize-sentences ["Are you ready, Tom?" "I want to go."]) => '(("Are" "you" "ready" "," "Tom" "?")
                                                                        ("I" "want" "to" "go" ".")))

(fact "Tokenize an input string, splitting sentences along the way"
      (tokenize "Are you ready, Tom? I want to go.") => '(("Are" "you" "ready" "," "Tom")
                                                          ("I" "want" "to" "go")))