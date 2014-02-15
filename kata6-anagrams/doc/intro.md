# Introduction to kata6-anagrams

[Kata 6](http://codekata.pragprog.com/2007/01/kata_six_anagra.html) is concerned with anagrams. An [anagram](https://en.wikipedia.org/wiki/Anagram) is a word that consists of characters which, when combined in a different order, form a different word. Now, when I started out with this kata, I was sitting on a train without internet connection, so I just went ahead with what I remembered from a quick glance over the kata description I had done the week before. So I thought that all that needed to be solved was to determine whether two words were anagrams of each other. My initial idea how to solve this is to generate the sets of the characters of both words and compare those:

	(defn remove-blanks [word]
         (str/replace word " " ""))

	(defn anagram-set? [word1 word2]
         (let [w1 (remove-blanks word1)
	           w2 (remove-blanks word2)] 
		   (= (set w1) (set w2))))

This time, I opted for using [midje](https://github.com/marick/Midje) for running the tests, in particular due to the possibility to run tests continually via [lein-midje](https://github.com/marick/lein-midje). Midje takes a slightly different approach / syntax to writing tests, adding the notion of _facts_ that are then verified. I.e. tests with midje look like this:

	(facts "Testing the set implementation for checking anagrams"
       (fact "Set anagram can find anagrams"
             (anagram-set? "the law" "wealth") => true)
       
       (fact "Set anagram is too simplistic"
             (anagram-set? "the lalalaw" "wealth") => true))

You can already see from the latter fact what is wrong with the initial solution: it's too simplistic with regard to handling the number of occurences of some character. (Some might say, the introduction of `remove-blanks` also is too complicated, but I wanted to handle [Anne Clark's "The law is an anagram of wealth"](http://www.lastfm.de/music/Anne+Clark/THE+LAW+Is+An+Anagram+of+WEALTH).

When I finally had some more time to read the kata description more carefully, I recognized that the task actually is to find all anagrams of a given word, checking back against a given wordlist. So that means that the kata consists of two tasks: generate all possible combinations for a given character sequence and check in this wordlist whether some candidate character sequence amounts to a known word. Now, if you take a step back, it's easy to see that anagrams are nothing else than permutations of the elements of a given (character) sequence, with the additional restriction that all such permutations must be (known) words again. So, we end up with a skeleton which looks like this:

	(defn generate-anagrams [word]
	   "Generate all anagrams of word"
	   (generate-permutations word))

	(defn find-anagrams [word words]
	   "Finds all anagrams of word in (the sequence of) words"
	   (let [anagrams (generate-anagrams word)
             wordset (set words)]
		 (loop [candidates anagrams
	            result []]
			(if (empty? candidates)
				result
				(recur (rest candidates)
				(if (and (not (= (first candidates) word))
					     (contains? wordset (first candidates)))
                    (concat result (list (first candidates)))
                    result))))))

Which now, of course leaves us with the task to implement a permutation algorithm. I must admit I had a pretty hard time to come up with something on my own without resorting to looking at other people's code. Given that the task of code katas is not primarily to invent algorithms on the fly, but to practice coding, I finally read the [wikipedia paragraph on computing permutations in lexicographic order](https://en.wikipedia.org/wiki/Permutation#Generation_in_lexicographic_order), which has a blue print of an algorithm which is attributed to Naranya Pandita, who invented it in the 14th century already. I took a very verbatim and top-down approach this time and ended up with this as the next piece of code:

	(defn next-permutation [squence]
	   (when-let [k (find-largest-index-with-bigger-successor squence)]
		    (let [l (find-largest-index-bigger-value squence k)
				  swapped (swap-positions squence k l)
				  current-perm (reverse-tail swapped (inc k))]
			  current-perm)))

	(defn generate-permutations [squence]
	   (let [start-perm (sort squence)]
	      (loop [permutation (next-permutation start-perm)
			     result (list start-perm)]
			 (if (or (not permutation)
				 (empty? permutation))
			 result
			 (recur (next-permutation permutation)
				    (concat result (list permutation)))))))

I went back to writing tests:

       (fact "finding the largest index with bigger successor"
             (find-largest-index-with-bigger-successor [1 2]) => 0
             (find-largest-index-with-bigger-successor [1 2 3 4]) => 2
             (find-largest-index-with-bigger-successor [1 2 4 3]) => 1
             (find-largest-index-with-bigger-successor [1 3 4 2]) => 1
             (find-largest-index-with-bigger-successor [1 4 3 2]) => 0)

The test results are taken straight out of the wikipedia article. Implementing this is pretty straight-forward: we just iterate through the list, keeping track of the current position and check whether the following element is bigger than the current element. If so, we keep the current position, otherwise we keep what we had so far as the result. When we reach the end of the sequence (or there is no subsequent to compare to), we have found the largest position (index) that has a successor with a bigger value. One thing is worth pointing out: the usage of `(comp pos? compare)` is necessary because `>` does only work on numbers, but no on characters (or keywords). Why Clojure does not follow Python (which provides a general purpose operators, which use something like `compare` under the hood which you can override for your data types) in this aspect is beyond me.

	(defn find-largest-index-with-bigger-successor [squence]
	     (loop [restsq (seq squence)
			    curpos 0
                curresult nil]
			(cond (or (empty? restsq)
				  (empty? (rest restsq)))
				  curresult
				  ((comp pos? compare) (second restsq) (first restsq))
				  (recur (rest restsq)
					     (inc curpos)
						 curpos)
	              :else
				  (recur (rest restsq)
					     (inc curpos)
						 curresult))))

The next step is finding the position of some value that is bigger than the position that we just determined. Again this is straight-forward:

       (fact "finding the largest index that has a bigger value than some other position"
             (find-largest-index-bigger-value [1 2 3 4] 2) => 3
             (find-largest-index-bigger-value [1 2 4 3] 1) => 3
             (find-largest-index-bigger-value [1 3 4 2] 1) => 2
             (find-largest-index-bigger-value [1 4 3 2] 0) => 3)

	    (defn find-largest-index-bigger-value [squence index]
			(let [compval (nth (vec squence) index)]
				(loop [restsq (seq squence)
					   curpos 0
					   curresult nil]
	              (cond (empty? restsq)
				  curresult
				  ((comp pos? compare) (first restsq) compval)
				  (recur (rest restsq)
					     (inc curpos)
						 curpos)
	              :else
				  (recur (rest restsq)
					     (inc curpos)
						 curresult)))))

We now have to swap these two elements which is easy enough to do with vectors:

       (fact "swapping two positions in a sequence"
             (swap-positions [1 2 3 4] 2 3) => [1 2 4 3]
             (swap-positions [1 2 4 3] 1 3) => [1 3 4 2]
             (swap-positions [1 3 4 2] 1 2) => [1 4 3 2]
             (swap-positions [1 4 3 2] 0 3) => [2 4 3 1])

	   (defn swap-positions [squence k l]
	       (let [seqvec (vec squence)]
			   (assoc (assoc seqvec k (nth seqvec l))
				   l (nth seqvec k))))

I first fiddled around with `take` and `drop` to avoid converting the input sequence to a vector but this makes the code much more complex. Why there is no general-purpose position-based replace is, again, beyond me -- there are a number of discussions around (the lack of) a general-purpose `subsequence` function which point out issues with complexity (code-and performance-wise), but I doubt that most manually-crafted workarounds lead to any better solutions. Maybe I'm missing something obvious here.

Next, we need to reverse the rest of the sequence behind the position which we just swapped. The example in the wikipedia article is not entirely clear for longer remainders, but some tests revealed that the right position is really the one we just used, like this:

       (fact "reverse the tail of a sequence"
             (reverse-tail [1 2 4 3] 2) => [1 2 3 4]
             (reverse-tail [1 3 4 2] 1) => [1 2 4 3]
             (reverse-tail [1 4 3 2] 0) => [2 3 4 1])

	    (defn reverse-tail [squence tail-position]
			(let [prefix (take tail-position squence)
				  tail (drop tail-position squence)
				  revtail (reverse tail)]
			  (concat prefix revtail)))

So, with this we now have all pieces in our hands and can test the entire algorithm:

       (fact "finding the next permutation"
             (next-permutation [1 2 3 4]) => [1 2 4 3]
             (next-permutation [1 2 4 3]) => [1 3 2 4])

Which will, surprise, surprise, give the expected results. So, with this we are able to generate all 24 permutations of `[1 2 3 4]` and we can go back to our anagram task.
Turns out that the tests would fail: I hadn't thought about the fact that the destructuring of the character sequence (i.e. the word) would require subsequent combination of the permutation results. That's easy enough to correct by `apply`ing `str` to all permutation results.

	(defn generate-anagrams [word]
	    (map (partial apply str) (generate-permutations word)))

Now, when you run this code with the test data given in the original kata:

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

I ran into a StackOverflowException for "boaster" though. Looking at the code, it's immediately obvious that there the only possible cause for this can be in `generate-permutations` which generates the result eagerly. So, let's change that to a lazy variant.

	(defn- gen-perms [squenze]
		(lazy-seq
			(when-let [permutation (next-permutation squenze)]
				(cons permutation (gen-perms permutation)))))

	(defn generate-permutations [squence]
		(let [start-perm (sort squence)]
			(cons start-perm (gen-perms start-perm))))

I use an external helper here because we need to add the start permutation to the final result up-front and that doesn't lend itself to a self-recursive function. 
Anyway, this concludes the first solution using a rather traditional algorithm.

For the next solution, I intended to use something else. I had the chance to hear David Nolen talk about [core.logic](https://github.com/clojure/core.logic) which reminded me a lot of the old days in which I was using [Prolog](https://en.wikipedia.org/wiki/Prolog) for computational linguistics and logic programming. In particular I was thinking of a permutation implementation in Prolog described in Richard O'Keefe's [Craft of Prolog](https://mitpress.mit.edu/books/craft-prolog), which I briefly discuss below:

    permutation(Xs, Ys) :-
    	permutation(Xs, Ys, Ys).
    permutation([],[],[]).
    permutation([X|Xs], Ys1, [_|Bound]) :-
    	permutation(Xs, Ys, Bound),
    	insert(Ys, X, Ys1).
    insert(L, X, [X|L]).
    insert([H|T], X, [H|L]) :-
    	insert(T,X,L).

If you would want to generate all permutations for a list `[1,2,3]`, you would call `permutation([1,2,3],Q)` and your Prolog interpreter of choice (e.g. [SWI-Prolog](http://www.swi-prolog.org/)) would generate the first possible result for Q and via backtracking generate all other possible permutations.

	?- permutation([1,2,3],Q).
	Q = [1, 2, 3] ;
	Q = [2, 1, 3] ;
	Q = [2, 3, 1] ;
	Q = [1, 3, 2] ;
	Q = [3, 1, 2] ;
	Q = [3, 2, 1].

Let's briefly discuss the Prolog solution, this will make it easier to discuss some issues when translating this to core.logic later on. Prolog uses _facts_ and _rules_ to prove some query. E.g., `permutation([],[],[]).` is a fact asserting that the permutation of an empty list is the empty list. Anything involving `:-` is a rule. Prolog uses [unifiction](https://en.wikipedia.org/wiki/Unification_(computer_science)) -- hang on, you'll see in a second what this is. Second, you see all those `[X|Xs]` constructions. These are basically list (de-)construction operations: they split off the first element or add an element (_head_) and some rest (_tail_) to form a new list. The point here is that if you're calling `permutation([1,2,3],Q,Q)` Prolog will try to _unify_ `[1,2,3]` with `[X|Xs]` which is possible when `X=1` and `Xs=[2,3]`. The `_` construct means "ignore", "don't care". If we consider only the `insert` fact (i.e. the first statement), this fact can be used by Prolog via unification to answer queries about any value of the predicate:

	?- insert([2,3],1,Q).
	Q = [1, 2, 3] 
	?- insert([2,3],Q,[1,2,3]).
	Q = 1 
	?- insert(Q,1,[1,2,3]).
	Q = [2, 3] 

The key to understand how `permutation` works is considering how `insert` works: the `insert` rule will deconstruct the first argument (assuming it's a list) and insert the second argument to it. This way, `X` will be inserted in all possible positions of the list:

	?- insert([2,3],1,Q).
	Q = [1, 2, 3] ;
	Q = [2, 1, 3] ;
	Q = [2, 3, 1].





