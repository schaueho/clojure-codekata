# Introduction to kata14-trigrams

[Kata 14](http://codekata.com/kata/kata14-tom-swift-under-the-milkwood/) is a semmingly simple one that is concerned with, as Dave Thomas puts it, "the heuristics of processing" texts, using [trigrams](https://en.wikipedia.org/wiki/Trigram) to produce (more or less) random new texts. 

Trigrams are not a new concept for me. Although the underlying concept is simple, they can be used for many interesting applications. Trigrams are a special case of N-grams, where N=3 turns out to be especially useful (as in "giving better results as other values for N") for natural language processing, at least for western languages. Nearly a decade ago, I had the pleasure to collaborate with some rather smart people who used trigrams to identify "matching" text snippets between dictionary entries. The idea was similar to what is described in this article on [using trigram vectors and nearest neighborhood calculation for document classification](http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.33.8765).As I'm generally interested in NLP and not only in doing coding katas, I will mainly focus on the trigram aspects in this kata, not so much on the random text generation part.

If you followed the link to the Wikipedia article, it's clear from the kata description that we need word-level trigrams, not character-level trigrams. The kata description also already reveals the data structure to use for solving the task, a `HashMap` and the algorithm is also described in enough detail to be straight-forward.

Let's augment the kata a little and decompose the tasks:

1. split some string into n-grams with default n=3, where we might want to apply different criteria to apply on where we can split the string (e.g. after each character or after each word)
2. parse a file into n-grams, where we need to consider sentence boundaries
3. parse a collection of files into n-grams concurrently (just to speed up parsing of a larger file collection and also to introduce another possibility to learn a little more about Clojure's specific tools to handle concurrency)
4. do some analysis on the trigrams found in the recommended _Tom Swift and his aircraft_ text
5. modify the n-gram computation to yield the "first two words as key / list of all third words value" map described in the kata
6. build a lazy-seq version of the text generation algorithm (because, as the example in the description already shows, there might be circles which could lead to infinite results) 
7. maybe implement the nearest neighborhood classification scheme described in the paper linked just for fun

But first things first: let's parse some string into trigrams. This, first of all, requires [tokenization](https://en.wikipedia.org/wiki/Tokenization). As a first obvious naive idea, we start out with simple string splitting, using `clojure.string`. First let's `split` on all whitespace `#"\s"`, using the first sentence in the Tom Swift text:

	kata14-trigrams.core> (str/split "Are you all ready, Tom?" #"\s")
	["Are" "you" "all" "ready," "Tom?"]

This already shows the issues surrounding punctuation that Dave Thomas mentions in the kata description. Basically, we have to consider what we want to do with sentence boundaries.  Fortunately, we're ultimately using [Java's Pattern class](http://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html), so we can also match (or split) on punctuation, although probably not on all punctuation, but only on those which signify a sentence boundary (i.e. the charset `[.!?]` followed by either whitespace `#"\s+"` or end of line `$`):

	kata14-trigrams.core> (str/split "Are you all ready - Tom?" #"\s*\p{Punct}\s*")
	["Are you all ready" "Tom"]
	kata14-trigrams.core> (str/split "Are you all ready, Tom?" #"\s*[.?!](\s+|$")
	["Are you all ready, Tom"]
	kata14-trigrams.core> (map #(str/split %1 #"\s+")
		                        (str/split "Are you all ready, Tom? I want to go."
				                           #"\s*[.!?](\s+|$)"))
	(["Are" "you" "all" "ready," "Tom"] ["I" "want" "to" "go"])

This still leaves the question open of what we want to do with the comma or any other interleaving punctuation. It's clear that we want to get rid of it somehow, but it's not too clear whether we would like to see "Tom" as a valid consecutive element in the text generation part. Probably not, so an idea here would be to try to make the remaining punctuation elements visible as separate tokens.

Let's put this issue aside and move on to the actual n-gram generation. Quite obviously, "computing" an n-gram is really just a simple sequence operation: you move through the sequence, always taking n elements as needed, until you're done. This is completely straight-forward to accomplish with a simple accumulator (`acc`) to collect the results that we `take` while looping through the sequence.

	(defn ngram
	  "Given a sequence sq and a number n, returns a sequence of new contiguous sequences
      of n items that appear in sq."
	  ([squence n]
	   (ngram squence n []))
	  ([squence n acc]
       (if-let [sq (seq squence)]
         (recur (rest sq) n (conj aux (take n sq)))
         acc)))
		 
Given that we might want to run this on longer strings (texts, books), it makes sense to make this lazy by wrapping the call to the accumulator version in `lazy-seq`.

	kata14-trigrams.core> (ngram [1 2 3 4 5 6] 3)
	((1 2 3) (2 3 4) (3 4 5) (4 5 6) (5 6) (6))
	kata14-trigrams.core> (realized? (ngram [1 2 3 4 5 6] 3))
	false
	kata14-trigrams.core> (take 2 (ngram [1 2 3 4 5 6] 3))
	((1 2 3) (2 3 4)

Okay, let's combine this with our `clojure.string/split` experiments:

	kata14-trigrams.core> (map #(str/split %1 #"\s+")
		                        (str/split "Are you all ready, Tom? I want to go."
				                           #"\s*[.!?](\s+|$)"))
	(["Are" "you" "all" "ready," "Tom"] ["I" "want" "to" "go"])
	kata14-trigrams.core> (map #(ngram %1 3) *1)
	((("Are" "you" "all") ("you" "all" "ready,") ("all" "ready," "Tom") ("ready," "Tom") ("Tom"))
	 (("I" "want" "to") ("want" "to" "go") ("to" "go") ("go")))

Okay, this looks like we've basically have everything we need in hand, now let's make it a little bit more formal.
First of all, the tokenization step. So far, we have basically done two things in one step, sentence boundary detection and in-sentence tokenization. I already hinted at the need to do further work on the in-sentence tokenization wrt. punctuation and there might be other steps that we might want to add in the future, for instance, [stemming](https://en.wikipedia.org/wiki/Stemming) or further [morphological analysis](https://en.wikipedia.org/wiki/Morphology_%28linguistics%29). I'll not go in the direction of a more thorough tokenization method, which would require to go beyond regular expressions for many languages, but at least let's communicate the intention of how the `tokenize` method works clearly.

	(fact "Tokenize an input string, splitting sentences along the way"
		  (tokenize "Are you ready, Tom? I want to go.") => '(("Are" "you" "ready" "," "Tom")
                                                            ("I" "want" "to" "go")))

	(defn tokenize
	  "Tokenize a string"
	  [string]
	  (-> string
	 	  (split-sentences)
		  (tokenize-sentences)))

This is basically the top-level function for tokenizing an incoming string, threading the result of splitting sentences into a tokenization function. Let's take a look at the details, which shouldn't be suprising at all. First we have `split-sentences`, which is basically `str/split` on sentence end markers. Then we have `split-on-whitespace`, which we've also already seen. `split-off-punctuation` is basically handling all punctuation not used up during sentence boundary detection, which we will want to keep. And then we have two wrappers `tokenize-sentence(s)` which do nothing more than handling mapping over the various bits and pieces. This concludes the tokenization step, *phew*.

	(fact "Split sentences in a string"
      (split-sentences "Are you ready, Tom? I want to go.") => ["Are you ready, Tom" "I want to go"])

	(defn split-sentences
	  "Split a string into a sequence of sentences"
	  [string]
	  (str/split string #"\s*[.!?](\s+|$)"))

    (fact "Splitting on whitespace"
      (split-on-whitespace "Are you  ready") => ["Are" "you" "ready"]
      (split-on-whitespace "Are") => ["Are"])

	(defn split-on-whitespace
	  "Take a string and split it's content on whitespace, removing the whitespace"
	  [string]
      (str/split string #"\s+"))

	(fact "Splitting but keeping punctuation if any"
      (split-off-punctuation "ready,") => ["ready" ","]
      (split-off-punctuation "ready") => ["ready"]
      (split-off-punctuation "!+#?") => [""])

	(defn split-off-punctuation
	  "Take a string and split it's content, keeping punctuation as new tokens"
	  [string]
	  (let [match (re-find #"(\w+)(\p{Punct})?" string)
            result (rest (keep identity match))]
		(if (seq result)
			result
			(vector ""))))

	(fact "Tokenize a sentence"
      (tokenize-sentence "Are you ready, Tom?") => '("Are" "you" "ready" "," "Tom" "?"))

	(defn tokenize-sentence
	  "Take a single sentence and return a sequence of tokens for it"
	  [sentence]
	  (flatten (map split-off-punctuation
                    (split-on-whitespace sentence))))

	(fact "Tokenize some sentences"
      (tokenize-sentences ["Are you ready, Tom?" "I want to go."]) => '(("Are" "you" "ready" "," "Tom" "?")
                                                                        ("I" "want" "to" "go" ".")))

	(defn tokenize-sentences
	  "Take a sequence of sentences and return a sequence of tokens for  each sentence"
	  [sentences]
	  (map tokenize-sentence sentences))

When combining this with the `ngram` function, this is already pretty close to what we'll need to solve the original kata, although we will need some further adjustment to the data structure, which I'm going to tackle later.

	kata14-trigrams.core> (map #(ngram %1 3)
		                       (tokenize "Are you ready, Tom? I want to go."))
	((("Are" "you" "ready") ("you" "ready" ",") ("ready" "," "Tom") ("," "Tom") ("Tom"))
	(("I" "want" "to") ("want" "to" "go") ("to" "go") ("go")))

So, let's move to the next part which is generating ngrams for an entire file. First of all, I think that again we better do this in a lazy fashion, no need to do lots processing of huge files that might not be completely necessary. Looking back we can see that for any given string `tokenize` processes the same string at least thrice: we're first splitting on sentence boundaries, then handle punctuation and finally split on whitespace. If you think about reading files, it's quite obvious that the `readLine` method of `java.io.BufferedReader` which is behind Clojure's `line-seq` is also processing buffers quite similarly looking for line ends to split on. Maybe, we can combine some of the work? Let's start out with figuring out how to process a file char by char lazily. An answer to a [stackoverflow question on processing files per character in Clojure](https://stackoverflow.com/questions/11669404/processing-a-file-character-by-character-in-clojure) strictly follows `line-seq`:

	(defn char-seq
	  [^java.io.Reader rdr]
	  (when-let [chr (.read rdr)]
		  (if (>= chr 0)
			  (cons (char chr) (lazy-seq (char-seq rdr))))))

This is a start but not too helpful as discussed in this other [stackoverflow thread on processing large text files](https://stackoverflow.com/questions/4118123/read-a-very-large-text-file-into-a-list-in-clojure/10462159#10462159), as the result of `line-seq` and `char-seq` is a cons and the lazy part of it doesn't help you much when you're not processing the file right away. Instead one might want to return a lazy sequence of results, closing the file only afterwards. This could like this:

	; cf. https://stackoverflow.com/questions/4118123/read-a-very-large-text-file-into-a-list-in-clojure/10462159#10462159
	(defn lazy-file-chars [file]
	   (letfn [(lfl-helper [rdr]
		          (lazy-seq
					  (if-let [chr (.read rdr)]
						  (when (> chr 0)
							  (cons (char chr) (lfl-helper rdr)))
						  (do (.close rdr) nil))))]
	      (lfl-helper (clojure.java.io/reader file))))

When you look at this simple piece of code, besides reading characters from disc and building up a lazy-seq, it's also a) doing a sanity check on the input and b) building up a particular structure to return. Sounds exactly like the hooks we might want to consider for parsing sentences on read. Let's rip the code apart and combine it with the guts of `split-sentences` (matching explicitly on characters instead of using regular expression character classes):

	(defn read-next-sentence [rdr aux]
	   (if-let [chr (.read rdr)]
		   (let [character (char chr)]
		     (cond (= \. character) aux
              (= \? character) aux
              (= \! character) aux
              (= \tab character) (recur rdr (conj aux \ ))
              :else (recur rdr (conj aux character))))
	   aux))
	   
	(defn file-sentences [file]
		(letfn [(lfs-helper [rdr]
			        (lazy-seq
						(if-let [sentence (seq (read-next-sentence rdr (vector)))]
							(cons (apply str sentence) (lfs-helper rdr))
							(do (.close rdr) nil))))]
	        (lfs-helper (clojure.java.io/reader file))))

`read-next-sentence` has some obvious deficancies: it now splits sentences on every occurance of `.?!`, not only on those occurences which are followed by whitespace. Second, it should handle (only) multiple occurances of `\return\newline` characters (CRLF) as sentence delimiters, too. Solving both of these issues requires to go in the direction of real parsers where we would have to see `aux` as a stack of previously read characters. And we might not only want to deal with tabs specially (turning them into a space), e.g. we might want to replace multiple spaces/tabs into a single space etc. I'll just draw a sketch here that we might want to elaborate further:

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

	(defn sentence-end-p [character charstack]
	    (cond (and (= character \space)
                   (some (partial = (peek charstack)) [\. \? \!])) true
              (and (= character \return)
                   (some (partial = (peek charstack)) [\. \? \!])) true
              (and (= character \newline)
                   (some (partial = (peek charstack)) [\. \? \!])) true
              (and (= character \newline)
                   (or (= (peek charstack) \newline)
                       (and (= (peek charstack) \return)
                            (= (peek (pop charstack)) \newline)))) true
              :else false))

	(defn next-char-result [character charstack]
	     (cond (and (empty? charstack)
			        (or (= character \space)
                    (= character \tab)
                    (= character \newline)
                    (= character \return))) charstack
               (and (= character \space)
				    (= (peek charstack) \space))  charstack
			   (and (= character \tab)
				    (= (peek charstack) \space))  charstack
	           (and (= character \tab)
			        (= (peek charstack) \tab))  (conj (pop charstack) \space) ; should never happen
			   (= character \tab) (conj charstack \space)
               (= character \return) charstack
               (and (= character \newline)
                    (= (peek charstack) \space)) charstack
               (and (= character \newline)
                    (= (peek charstack) \return)) (conj (pop charstack) \space) ; should never happen
               (= character \newline) (conj charstack \space)
               :else (conj charstack character)))

I'll leave it at that, although it's clear that we can and probably should extend it in many different ways. Here are the adapted functions
to use these:

	(defn read-next-sentence
	   ([rdr]
		   (read-next-sentence rdr (vector) (vector)))
	   ([rdr seen result]
	       (if-let [chr (.read rdr)]
			   (when (> chr 0)
				   (let [character (char chr)]
					   (if (sentence-end-p character seen)
						   result
						   (recur rdr (conj seen character)
							   (next-char-result character result)))))
			   result)))

	(defn file-sentences [file]
		(letfn [(lfs-helper [rdr]
			        (lazy-seq
						(if-let [sentence (seq (read-next-sentence rdr))]
							(cons (apply str sentence) (lfs-helper rdr))
							(do (.close rdr) nil))))]
			(lfs-helper (clojure.java.io/reader file))))

The result is here that we now have a `read-next-sentence` function which just reads (non-lazily) and a (local) helper function which uses it to build up lazy sequence of sentences. Let's test it briefly:

	kata14-trigrams.core> (pprint 
		                    (map #(ngram %1 3) 
							   (tokenize-sentences 
								  (take 2 
							        (file-sentences test-file)))))
	((("The" "Project" "Gutenberg")
	 ("Project" "Gutenberg" "EBook")
	 ("Gutenberg" "EBook" "of")
	 ("EBook" "of" "Tom")
	 ("of" "Tom" "Swift")
	 ("Tom" "Swift" "and")
     ("Swift" "and" "his")
     ("and" "his" "Airship")
	 ...

Although one would probably now integrate more functionality from `tokenize-sentences` into `read-next-sentence`, I'll won't elaborate this now and see task 2 as solved.

