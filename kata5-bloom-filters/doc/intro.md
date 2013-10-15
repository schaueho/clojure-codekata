# Introduction to kata5-bloom-filters

[Kata5](http://codekata.pragprog.com/2007/01/kata_five_bloom.html) i an interesting one to do in Clojure as Bloom filters, which are a probalistic data-structure for determining set membership, are all about spending as few bits as possible (or required) to store data.  When bit manipulations are required, not many programmers would jump to Lisp or Java and indeed most descriptions are about implementations in C or one of it's derivates. This probably does not come as a surprise, but as we will see is not entirely justified (wrt. to Lisp or Java).

But before discussing this in detail, let's dive in with this kata. The [description of the kata](http://codekata.pragprog.com/2007/01/kata_five_bloom.html) already tells us pretty exactly what we're supposed to build: a bunch of hash functions and an array of bits which are then set or checked. We'll start with the hash functions first. Hash functions are dime-a-dozen, Java provides one, Clojure, too. Still it is interesting to go beyond the readily provided functions and to implement some hashing functions. The goal here is not to build perfect hash functions, but to get a feel how an implementation of one looks like in Clojure (as this blog post series is about Clojure, not about Computer Science).

As the task is here to build a Bloom filter for strings, all hash functions basically boil down to iterating over a sequence of characters, converting each character into a numerical value (i.e. applying `int`) and then using this in an accumulating computation of the total hash value. We start out with more or less the simplest aproach possible: we simply sum up the integer values of the characters (also known as the Kernighan & Ritchie "lose-lose" hash algorithms).

      (defn sum-chars 
        "Sum up the chars of a given string"
     	[charseq]
     	(reduce + (map int charseq)))

Nothing interesting to see but a straight-forward `map`/`reduce`, so let's move on and take a stab at what Dan Bernstein (djb) suggested. In its most innocent version this iooks pretty similar, but uses bit-shifting and has some magic numbers thrown in for good measure.  

   	(defn djb-string-hash
     	 "Use djb's method for hashing a string"
     	 [charseq]
     	 (reduce (fn [curhash charval]
                   (+ 
                     (+ (bit-shift-left curhash 5) curhash) 
                   charval))
             (cons 5381 (map int charseq))))

This is already were it got interesting, because if you run this with the simple string "foobar", you will run directly into an overflow. The [clojure documentation on unchecked-add](http://clojure.github.io/clojure/clojure.core-api.html#clojure.core/unchecked-add) could have told me so directly, of course. I had to use a quite a bit of websearch-fu, wildly testing around and knocking my head on the table to come up with this version:

     	(defn djb-string-hash
       	  "Use djb's method for hashing a string"
          [charseq]
	  (reduce (fn [curhash charval]
	               ^long (unchecked-add 
           	               (unchecked-add (bit-shift-left curhash 5) curhash) 
			       charval))
		(cons 5381 (map int charseq))))

This looks quite good, but is still not bulletproof, as testing it with a longer string (> 11 characters) will show. The real issue is actually due to Java: `unchecked-add` uses internally a data structure that can (and will) result in a Java `IntegerOverflowException`. Java does not have unsigned numeric types and also defaults to throwing exceptions on overflow and Clojure, leveraging the JVM, is directly affected by that issue. 

          kata5-bloom-filters.core> (djb-string-hash "foobar")
	  6953516687550
     	  kata5-bloom-filters.core> (djb-string-hash "foobarfoobar")
     	  ArithmeticException integer overflow  clojure.lang.Numbers.throwIntOverflow (Numbers.java:1388)

Compare for instance the behavior of Common Lisp: the straight-forward translation of the exact same naive implementation will not overflow (unless agressively optimizing against safety or promising to the compiler that only certain results will occur) due to automated boxing. 

	(defun djb-string-hash (charseq)
	  "Use djb's method for hashing a string"
	  (reduce #'(lambda (curhash charval)
	  	      (+ (+ (ash curhash 5) curhash)
		         charval))
	       (cons 5381 (mapcar #'FIXME-CHARTOINT charseq))))

Clojure does provide a similar behavior with e.g. the `BigInteger` type, however this is of no use as Clojure's bit-operations don't know how to handle BigInteger data -- they are only defined for the primitive data types that Java provides. Worse, converting back from a BigInteger to a primitive data type (e.g. `long`) will not -- as one might naively expect -- simply truncate the data but yield -2:

	kata5-bloom-filters.core> (bit-shift-left (bigint 20) 2)
	IllegalArgumentException bit operation not supported for: class clojure.lang.BigInt  clojure.lang.Numbers.bitOpsCast (Numbers.java:1008)

	kata5-bloom-filters.core> (* 2 (bigint Integer/MAX_VALUE))
	4294967294N
	kata5-bloom-filters.core> (int (* 2 (bigint Integer/MAX_VALUE)))
	IllegalArgumentException Value out of range for int: 4294967294  clojure.lang.RT.intCast (RT.java:1115)
	kata5-bloom-filters.core> (unchecked-int (* 2 (bigint Integer/MAX_VALUE)))
	-2

Similar issues arose with all of the other hash functions (i.e. sdbm and fnv), cf. the [code on github](https://github.com/schaueho/clojure-codekata) -- nothing interesting here, so let's move on to the bloom filter itself. 

Multiple options come to mind when thinking about the data structure to use. Let's start out with the simplest possible option: just use a simple number. This defaults to Java long, i.e. a 64-bit number. This implies of course that we already have a rather arbitrary upper limit on the size of the bloom filter, which influences the number of possible entries and the number of false positives, cf. this overview article about the [garden variety of bloom filters](http://matthias.vallentin.net/blog/2011/06/a-garden-variety-of-bloom-filters/).

	(defn bloom-add [bloom charseq & {:keys [hashfns] :or {hashfns *hash-functions*}}]
	  (reduce #(bit-set %1 %2) 
              (conj (map #(% charseq) hashfns)
                    bloom)))

	(defn bloom-contains? [bloom charseq & {:keys [hashfns] 
	      		      	     	        :or {hashfns *hash-functions*}}]
	  (every? #(bit-test bloom %) 
	  	  (map #(% charseq) hashfns)))

	(defn build-bloom [wordfile & {:keys [hashfns] 
	      		  	       :or {hashfns *hash-functions*}}]
	  (reduce #(bloom-add %1 %2 :hashfns hashfns)
            (cons 0 (string/split-lines (slurp wordfile)))))

The code here is pretty straight-forward, maybe with the possible exception that we're mapping over a list of functions in `bloom-add` and `bloom-contains?`. We could extract this part to a simple function which makes the code a little more readable.

    	 (defn hash-string [charseq & {:keys [hashfns] :or {hashfns *hash-functions*}}]
	  (map #(% charseq) hashfns))

This very naive implementation will run into problems right away: The hash functions will yield hash values that are itself 64 bit in size whereas the biggest bit that can be set is 63. A straight-forward fix for that is to consider the size of the 'bloom filter' (i.e. 64 bit) and to truncate the hash values accordingly via the modulo operation. I.e., instead of calling `(bit-set bloom value)` we do `(bit-set bloom (mod value 64))`.

Now, if you think about it, using a simple number is probably not the optimal data stucture: for one, we just limited us to bit arrays that are 64 bits in size (which for instance implies that with the `/usr/share/dict/words` file you'll end up with `Integer/MAX_VALUE`, i.e. all bits set to 1) but due to the immutable nature of the mathematical operations, we actually require a lot more space than just one long, thereby very much defeating an important characteristic of Bloom filters.

So let's use a completely different idea and use one of Java's mutable data structures: BitSets. This leads to the following naive, non-thread-safe implementation:

   	(defn bloom-add [bloom charseq & {:keys [hashfns] :or {hashfns *hash-functions*}}]
     	    (let [size (.size bloom)]
     	      	 (doseq [hashval (hash-string charseq :hashfns hashfns)]
      	           (.set bloom (Math/abs (mod hashval size)) true))
   		 bloom))

	(defn bloom-contains? [bloom charseq & {:keys [hashfns] :or {hashfns *hash-functions*}}]
  	    (let [size (.size bloom)
	          hashvals (hash-string charseq :hashfns hashfns)]
    	       (every? #(= (.get bloom (Math/abs (mod % size))) true) hashvals)))

	(defn build-bloom [wordfile & {:keys [bloom-filter size hashfns]
                               :or {size 1024
                                    hashfns *hash-functions*}}]
  	    (let [bloom (or bloom-filter (BitSet. size))]
               (reduce #(bloom-add %1 %2 :hashfns hashfns)
                  (cons bloom (string/split-lines (slurp wordfile))))
	        bloom))

We basically just exchanged the `bit-set`/`bit-test` functions with the respective BitSet methods and use a dynamic size. This hints at a possible generalization: we could consider multiple  bloom filter implementations (types, if you want to) that need to support some sort of bit-setting and getting operation plus size. This would be the internal protocol, while `bloom-add`/`bloom-contains?` (and maybe `build-bloom`) form the external API.

Now, of course, we would like to fix the problem that this code is not thread-safe. As is made pretty clear in Fogus etal. book "Joy of Clojure", Clojure's reference types are of no use here:
> "Wrapping a mutable object in a Clojure reference type provides absolutely no guarantees for safe concurrent modification. Doing this will at best explode immediately or, worse, provide inaccurate results."

The advice Fogus etal. offer is to use the `locking` macro. If we combine the above idea of using an internal protocol, we can at least apply it where it is necessary, i.e. around the calls to `.get`/`.set`:

     (defmulti bloom-size type)
     (defmethod bloom-size BitSet [bitset]
       (.size bitset))

     (defmulti bloom-bit-get 
       (fn [bloomfilter position]
          (type bloomfilter)))
     (defmethod bloom-bit-get BitSet [bitset position]
       (locking bitset
          (.get bitset position)))

     (defmulti bloom-bit-set 
       (fn [bloomfilter position value]
          (type bloomfilter)))
      (defmethod bloom-bit-set BitSet [bitset position value]
       (locking bitset 
          (.set bitset position value)))
	






