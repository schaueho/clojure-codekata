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

(fact "Test for sentence end"
      (sentence-end-p \space   [\g \o \.])                    => true
      (sentence-end-p \space   [\r \e \a \d \y \?])           => true
      (sentence-end-p \newline [\y \return \newline \return]) => true
      (sentence-end-p \newline [\y \newline])                 => true
      (sentence-end-p "B"      [\.])                          => false
      (sentence-end-p \newline [\y \return])                  => false
      (sentence-end-p \newline [\y])                          => false)

(fact "Parse result for characters depends on previous reads"
      (next-char-result \space   [\g \o \space])  => [\g \o \space]
      (next-char-result \tab     [\g \o])         => [\g \o \space]
      (next-char-result \tab     [\g \o \space])  => [\g \o \space]
      (next-char-result \tab     [\g \o \tab])    => [\g \o \space]
      (next-char-result \return  [\g \o])         => [\g \o]
      (next-char-result \newline [\g \o \return]) => [\g \o \space]
      (next-char-result \newline [\g \o])         => [\g \o \space])

(fact "Generate a map from (n-m) prefixes to suffixes"
      (ngram-mapset '[[1  2 3] [2 3 4] [1 2 4]] 2) => {'(2 3) #{4}, 
                                                           '(1 2) #{3 4}}
      (ngram-mapset '(("I" "wish" "I") ("wish" "I" "may") 
                          ("I" "may" "I")  ("may" "I" "wish") 
                          ("I" "wish" "I") ("wish" "I" "might") 
                          ("I" "might") ("might")) 2) => {'("might") #{}, 
                                                          '("I" "might") #{}, 
                                                          '("may" "I") #{"wish"}, 
                                                          '("I" "may") #{"I"}, 
                                                          '("wish" "I") #{"may" "might"}, 
                                                          '("I" "wish") #{"I"}})
