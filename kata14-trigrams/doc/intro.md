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

`read-next-sentence` has some obvious deficiancies: it now splits sentences on every occurance of `.?!`, not only on those occurences which are followed by whitespace. Second, it should handle (only) multiple occurances of `\return\newline` characters (CRLF) as sentence delimiters, too. Solving both of these issues requires to go in the direction of real parsers where we would have to see `aux` as a stack of previously read characters. And we might not only want to deal with tabs specially (turning them into a space), e.g. we might want to replace multiple spaces/tabs into a single space etc. I'll just draw a sketch here that we might want to elaborate further:

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

I'll leave it at that, although it's clear that we can and probably should extend it in many different ways. Here are the adapted functions to use these:

	(defn read-next-sentence
	   ([rdr]
		   (read-next-sentence rdr (vector) (vector)))
	   ([rdr seen result]
	       (let [chr (.read rdr)]
			   (if (and chr
			            (>= chr 0))
				   (let [character (char chr)]
					   (if (sentence-end-p character seen)
						   result
						   (recur rdr (conj seen character)
							   (next-char-result character result))))
			       result))))

	(defn read-sentences [x]
		(letfn [(lfs-helper [rdr]
			        (lazy-seq
						(if-let [sentence (read-next-sentence rdr)]
							(cons (apply str sentence) (lfs-helper rdr))
							(do (.close rdr) nil))))]
			(lfs-helper (clojure.java.io/reader x))))

The result is here that we now have a `read-next-sentence` function which just reads (non-lazily) and a (local) helper function which uses it to build up lazy sequence of sentences. Let's test it briefly:

	kata14-trigrams.core> (pprint 
		                    (map #(ngram %1 3) 
							   (tokenize-sentences 
								  (take 2 
							        (read-sentences test-file)))))
	((("The" "Project" "Gutenberg")
	 ("Project" "Gutenberg" "EBook")
	 ("Gutenberg" "EBook" "of")
	 ("EBook" "of" "Tom")
	 ("of" "Tom" "Swift")
	 ("Tom" "Swift" "and")
     ("Swift" "and" "his")
     ("and" "his" "Airship")
	 ...

Although one would probably now integrate more functionality from `tokenize-sentences` into `read-next-sentence`, I'll won't elaborate this now and see task 2 as solved. As a side note, this looks as if it's only restricted to files now, but it really isn't, as `clojure.java.io/reader` will happily accept `StringReader` arguments:

	kata14-trigrams.core> (import java.io.StringReader)
	java.io.StringReader
	kata14-trigrams.core> (take 2 (read-sentences (StringReader. "This is a sentence. And another one")))
	("This is a sentence." "And another one")

Taking a stab at task 3, concurrent processing of input files, we could imagine several scenarios. One could be in which we're simply parsing multiple files into a single trigram collection, hopefully in parallel. Another one could be in which we want to handle more and more files over time, e.g. because users can add more files interactively (web app or something). Let's tackle the first scenario first, using futures. The following code simply wraps our little test function from above into `future` calls. We trigger the parallel computation via `doall` (which would otherwise be delayed until dereference due to the implicit lazyness):

	(defn inputs-to-future-ngrams 
		"Convert a sequence of reader-readable inputs (files, urls, etc.) into future ngrams"
		[inputs n]
		(doall
			(map 
				#(future
					(map 
						(fn [tokens] 
							(ngram tokens n))
						(tokenize-sentences (read-sentences %1))))
	            inputs)))

	kata14-trigrams.core> (let [futsent (inputs-to-future-ngrams [swift-file] 3)]
		                       (doseq [f futsent]
						           (pprint (take 2 @f))))
	((("The" "Project" "Gutenberg")
	  ("Project" "Gutenberg" "EBook")
	  ("Gutenberg" "EBook" "of")
	  ("EBook" "of" "Tom")
	  ("of" "Tom" "Swift")
	  ("Tom" "Swift" "and")
	  ...


Let's move further on with the original task which ultimately aims to produce random text from the trigrams computed from the original one. His main approach for this is to take a generated word pair A B (like a window of the last two words you've generated so far) and figure out what the next word would be by checking whether there is a trigram which starts with A B. He even suggests building a table which provides a mapping from all such pairs of A and B to some C such that A B C are a trigram in the original text. His suggestion looks a lot like being Ruby inspired and uses lists, but in Clojure we want to generate a map where the keys are a list of length 2 and the value is a set. For any pair A B, we can then select randomly from the set. So, we have to convert our currently plain vector of trigrams, grouping it by the first two words and collect the remaining third value. Now, although I said 'group by', `group-by` is not of much use here. 

Let's start simple by splitting a given trigram: we just use `split-at`. Next, we will update a map using `assoc`. However, we need to check whether we have already some value for the prefix (i.e. our word pair): if so, we assume the value is a set to which we want to add our new suffix (set union), otherwise we just convert the returned suffix (a list, as returned from `split-at`) to a set.

	kata14-trigrams.core> (split-at 2 '("wish" "I" "may"))
	[("wish" "I") ("may")]
	kata14-trigrams.core> (let [mymap {'("wish" "I") #{"may"}}
		                        newtri '("wish" "I" "might")
     			                [k v] (split-at 2 newtri)]
			                    (prn "Key: " k "Value:" v)
			                (if (get mymap k nil)
			                    (assoc mymap k (union (get mymap k) v))
			                    (assoc mymap k (set v))))
    "Key: " ("wish" "I") "Value:" ("might")
    {("wish" "I") #{"may" "might"}}
	kata14-trigrams.core> (let [mymap {'("wish" "I") #{"may"}}
		                        newtri '("a" "new" "try")
     			                [k v] (split-at 2 newtri)]
			                    (prn "Key: " k "Value:" v)
			                (if (get mymap k nil)
			                    (assoc mymap k (union (get mymap k) v))
			                    (assoc mymap k (set v))))
	"Key: " ("a" "new") "Value:" ("try")
	{("a" "new") #{"try"}, ("wish" "I") #{"may"}}		

This is the core part, now we just throw a `reduce`/`map` around it and stream-line the code a little, to finally end up with this small function:

	(defn ngrams2prefixmap
	   "Takes a collection of ngrams and returns a map with prefixes of size 'prefixlength' to remainders."
       [ngrams prefixlength]
	   (reduce (fn [m [k v]]
                   (assoc m k (union (get m k #{}) (set v))))
			       (hash-map)
			    (map #(split-at prefixlength %) ngrams)))

I don't know whether this is really useful, but I have the impression there is something more generally useful embedded in here, so let's extract it and reformulate a little.

	(defn collect-mapset
	  "Call `f` on collection `coll` which is assumed to return a collection of key value pairs and groups this collection into a map `m` from keys to a set of all values for any given key."
	  ([f coll]
        (collect-mapset f coll (hash-map)))
	  ([f coll m]
         (reduce 
              (fn [m [k v]]
                     (assoc m k (union (get m k #{}) (set v))))
	          m
              (f coll))))

	 (defn ngram-mapset
      [ngrams pl]
      (collect-mapset 
         (fn [coll] 
             (map #(split-at pl %1) coll)) 
         ngrams))

     (fact "Takes n grams to generate a map from prefixes to a set of suffixes"
       (ngram-mapset '[[1  2 3] [2 3 4] [1 2 4]] 2) => {'(2 3) #{4}, 
                                                        '(1 2) #{3 4}}
       (ngram-mapset '(("I" "wish" "I") ("wish" "I" "may") 
                       ("I" "may" "I")  ("may" "I" "wish") 
                       ("I" "wish" "I") ("wish" "I" "might")) 
                      2) => {'("may" "I") #{"wish"}, 
                             '("I" "may") #{"I"}, 
                             '("wish" "I") #{"may" "might"}, 
                             '("I" "wish") #{"I"}})


For generating text randomly, Clojures `rand-nth` will provide us with the main functionality, which we can use to randomly select an element from a collection:

	kata14-trigrams.core> (rand-nth (keys (ngram-mapset (ngram (first (tokenize "I wish I may I wish I might")) 3) 2)))	     
	("wish" "I")
	kata14-trigrams.core> (rand-nth (keys (ngram-mapset (ngram (first (tokenize "I wish I may I wish I might")) 3) 2)))	     
	("I" "wish")
	kata14-trigrams.core> (rand-nth (keys (ngram-mapset (ngram (first (tokenize "I wish I may I wish I might")) 3) 2)))	     
	("I" "might")

The generation function is fairly straight-forward. We use an accumulator to gather the result which we will convert to a string when one of the two end conditions is met: either the result is longer than a `maxlength` or for a given word pair (the `prefix`, which is just the last two words from what we've got so far) we cannot find a trigram A B C, so that C would be our next new word. For the start, we just take a random key from the generated map and also use it in the accumulator `result`.
 
	(defn generate-random-text-helper [mapset ml result]
		(if (>= (count result) ml)
			(str/join " " result)
			(let [prefix (take-last 2 result)
				  suffixes (get mapset prefix [])
				  newword (and (seq suffixes) (rand-nth (seq suffixes)))]
			  (if newword
				  (generate-random-text-helper mapset ml (conj result newword))
				  (str/join " " result)))))

	(defn generate-random-text [ngrams n maxlength]
		"Generate a random text from a collection of ngrams. Will stop when we've reached a dead-end or the text reaches maxlength."
		(let [ngrammap (ngram-mapset ngrams (dec n))
			  key (rand-nth (seq (keys ngrammap)))
			  suffixset (get ngrammap key [])
			  newword (and (seq suffixset) (rand-nth (seq suffixset)))]
		  (generate-random-text-helper ngrammap maxlength (conj (vec key) newword))))

Running some analysis on the Swift text, I have 4482 "sentences" in the text and 24247 keys in the generated 2-gram map. Also interesting is the list below which shows the length distributions in the map: e.g. we have 17925 entries in the map which for the respective key will have only one possible following word and 2 entries (keys) for which our random generator can chose from 60 possible followup words. You'll see the different sets for length 17 below. 

	kata14-trigrams.core> (def swiftgrams (mapcat #(ngram %1 3)
	  		                (tokenize-sentences 
				             (take 4482
				                (read-sentences swiftfile)))))
    #'kata14-trigrams.core/swiftgrams
    kata14-trigrams.core> (count (keys (ngram-mapset swiftgrams 2)))
    24247
	kata14-trigrams.core> (pprint (sort-by first (map (fn [[length entries]] [length (count entries)]) (group-by count (vals (ngram-mapset swiftgrams 2))))))
	([0 1444]
	[1 17925]
	[2 2538]
	[3 893]
	... elided ...
    [60 2]
    ...
    kata14-trigrams.core> (get (group-by count (vals (ngram-mapset swiftgrams 2))) 17)
    [#{"finally" "I" "for" "storm" "promised" "was" "said" "mused" "really" "however" "retorted" "as" "and" "young" "though" "to" "remarked"} #{"were" "escaped" "be" "there" "in" "yet" "I" "had" "," "we" "at" "mingled" "and" "he" "belonged" "who" "you"} #{"a" "started" "what" "back" "that" "em" "worse" "any" "alarmed" "below" "the" "nervous" "half" "even" "to" "right" "on"} #{"but" "Mr" "from" "put" "Tom" "went" "wait" "and" "agreed" "replied" "cried" "declared" "when" "after" "he" "Higby" "admitted"} #{"swiftly" "no" "that" "they" "I" "was" "buoyant" "it" "we" "slowly" "the" "as" "Tom" "far" "to" "he" "you"} #{"help" "stop" "start" "come" "take" "pay" "go" "do" "depend" "make" "serve" "give" "tell" "believe" "avoid" "stay" "scour"} #{"a" "Mr" "active" "yells" "its" "sleeping" "huge" "enthusiasm" "nothing" "the" "Tom" "flashing" "nervous" "his" "sure" "documents" "something"} #{"road" "start" "advertisement" "view" "one" "place" "plan" "thrashing" "-" "thing" "tackle" "notion" "landing" "idea" "ship" "turn" "loser"}]

Now, let's run the generator a few times and see what it produces:

	kata14-trigrams.core> (generate-random-text swiftgrams 3 120)
	"representations concerning the copyright holder found at the latest mechanical affair in which Ned Newton , but too much it will take an extra propeller along after this task had been obtained , Tom ! called Miss Delafield ."
	kata14-trigrams.core> (generate-random-text swiftgrams 3 120)
	"tracks . "
	kata14-trigrams.core> (generate-random-text swiftgrams 3 120)
	"helping Mr ."
	kata14-trigrams.core> (generate-random-text swiftgrams 3 120)
	"the edges by walking around ."
	kata14-trigrams.core> (generate-random-text swiftgrams 3 120)
	"managing it , you haven ' you do something ?"
	kata14-trigrams.core> (generate-random-text swiftgrams 3 120)
	"we drop down on them before and , carrying a heavy load ."
	kata14-trigrams.core> (generate-random-text swiftgrams 3 120)
	"once the falling motion had been making some calculations regarding wind pressure , that had taken ."
	kata14-trigrams.core> (generate-random-text swiftgrams 3 120)
	"stormed the man turned off the airship that will go , of the next day on fitting up the brace and mounted his wheel , cried the sheriff having seen Tom Swift will wish he ' already laid claim to the village itself we can continue our journey , and play it all right ?"

I was actually lucky this time, quite often I run into dead ends (as seen in my second and third try), which is not too surprising given how the data in the map is distributed. But still we got some silly sentences -- kata solved.
