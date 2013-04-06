# Introduction to kata2-chop-binary-search

The first real programming task on CodeKata is [Kata 2, Karate Chop](http://codekata.pragprog.com/2007/01/kata_two_karate.html "http://codekata.pragprog.com/2007/01/kata_two_karate.html"). Or as the introduction says: 

> A binary chop (sometimes called the more prosaic binary search) finds the position of value in a sorted array of values.

I started out with the simplest approach I could think of: there just might be a function already readily available in Clojure which solves the problem. After all, Common Lisp has 'position' whereas Pythonistas would use the 'index' method on lists. I couldn't really find a function on clojure sequences which would immediately take care of the issue, but 'keep-indexed' seems close enough -- taking a function and a collection, it calls the function which ought to take an index and a value, and keeps the function's returned non-nil values. This led to the following code:

	(defn chop [x coll]
		(let [result
			    (keep-indexed
				   (fn [idx item]
				     (when (= item x) idx))
  				   coll)]
			(if (empty? result)
                -1
              (first result))))

Some points may be of interest here:

- Looking over some code, I ran into the usage of %1 etc. to refer to implicit arguments, which I didn't knew about when I started out. This version screams 'Common Lisp' pretty much all over.
- I also ran into the empty sequence <> false issue, of course and had to look up `empty?` as well.
- Using `when` instead of `if` when you only care about the true state is an idiom I knew and love from CL. The `if` part at the end is still ugly. We could get rid of it by relying on `first` on an empty sequence to return nil and using this in a boolean comparison.

This leads to the following much shorter version:

	(defn chop [x coll]
		(let [result (keep-indexed #(when (= %2 x) %1) coll)]
		(or (first result) -1)))

The next idea is to use a multi-method approach, dispatching on either values, possibly empty ones and on type, of course. This is an approach which I think should be possible with CLOS, but is quite
outside of mainstream object-oriented languages like Java or Python. We could combine this with a recursive approach. One base case of the recursion would be the empty collection, of course, with the other one being finding the searched value, returning the current index in the sequence which we have to carry around (straight forward recursion). ClojureDocs example 683 has a nice blue print (<http://clojuredocs.org/clojure_core/clojure.core/defmulti#example_683>). The result has a nice declarative touch to it, which reminds me of my old Prolog days: 

    (derive clojure.lang.Sequential ::collection)
    (defmulti chop-helper
      (fn [x coll idx]
        (if (empty? coll) coll
            (type coll))))
    (defmethod chop-helper [] [x coll idx]  -1)
    (defmethod chop-helper ::collection [x coll idx]
      (if (= x (first coll))
        idx
        (chop-helper x (rest coll) (inc idx))))
    (defmethod chop-helper :default [x coll] :oops)
    (defn chop [x coll]
      (chop-helper x coll 0))
	
Figuring this one out required me to understand that Clojure really uses a pretty normal function for the dispatch in the `defmulti` declaration, whereas I was reading the first code examples quite similar to CL's more simple argument type and value dispatch. As I wanted to dispatch on value (empty collection) and on type (collection), I need to setup an according dispatch function. I am still quite unaware of Clojures ad hoc hierachy system, so I'm not convinced that my use of derive and henceforth use of this 'type' couldn't be simplified.


The alert reader might have noticed that so far, although all solutions gave correct results on the tests provided, none of the implementations was actually correct when it comes to sticking to the task description, which clearly refers to not only implement /some/ search, but a binary one. To put it otherwise: the first solutions are actually implementations of a linear search. Binary search amounts to a simple divide and conquer (search) strategy: you compare the value in the middle, and if what you looked for is smaller, you repeat with the lower half of the collection, otherwise you repeat with the upper part of the collection. This, of course, assumes that the collection is already sorted and is hence somewhat less general than linear search. In addition, for this strategy to be efficient (ie. to really require only O(log2(n)) with n=length of the collection), you need to be able to access elements directly and don't have to traverse the collection to find out its length. Hence, I'll use vectors and vector specific functions this time. 

I'll use a recursive approach again, however, this time using the `recur` version that clojure provides for optimizing tail-recursive calls. And here we can make use of the arity-variadic definition of functions, so we don't need the helper 'function' like with the multimethod approach above.

    (defn chop
       ([x vec]
         (chop x vec 0))
       ([x vec acc]
         (let [len (count vec)
               middle (Math/round (Math/floor (/ len 2)))]
           (cond (empty? vec) -1
                 (= x (nth vec middle))
                    (+ acc middle)
                 (and (< x (nth vec middle))
                      (>= middle 0))
                    (recur x (subvec vec 0 middle) acc)
                 (and (> x (nth vec middle))
                      (<= (inc middle) len))
                    (recur x (subvec vec (inc middle) len) (inc middle))))))

 
I actually also had another version using a default value for the accumulator parameter: on reflection, I can see that this is my mind still set in the 'a function always has a fixed number of arguments' mindset, in combination with optional arguments. The approach is taken from a tip on stackoverflow and uses map destructuring of an optional argument (<http://stackoverflow.com/questions/3208347/how-to-create-default-value-for-function-argument-in-clojure>):

    (defn chop [x vec & {:keys [acc] :or {acc 0}}]
      (let [len (count vec)
            middle (Math/round (Math/floor (/ len 2)))]
        (cond (empty? vec) -1
              (= x (nth vec middle)) (+ acc middle)
              (and (< x (nth vec middle))
                   (>= middle 0))
                 (recur x (subvec vec 0 middle) {:acc acc})
              (and (> x (nth vec middle))
                   (<= (inc middle) len))
                 (recur x (subvec vec (inc middle) len) {:acc (inc middle)}))))

This is somewhat ugly because you have to use a map with recur, although otherwise you could call the function directly with key value alone (i.e. `(chop 2 [1 2 3] :acc 0)`, due to the restrictions on arity matching with `recur` (cf. <http://clojure.org/special_forms?Special%20Forms--%28recur%20exprs*%29>).

The next version to think of is a non-recursive one. This, of course, leaves one a little with scratching your head, since the standard iterative solution to binary search uses start and end indices with re-occuring assignments of values until a match has been found or the start value exceeds the end. Now, without changing assignment to locals, this doesn't sound like to easy to simulate. However, using `loop` and `recur` this is actually very easy to simulate: loop provides the local variables for start and end values and we will recur to this target. This sounds a bit like cheating (i.e. like using a recursive version all over again) but actually we're only assigning new values to the 'start' and 'end' variables during each iteration and given that recur is for tail recursion only, I would assume that the implementation is actually just a simple jump and assignment under the hood. Interestingly enough, this one worked on first try, whereas I actually encountered some of the one-off errors that is mentioned in the original description of the kata with the recursive version. Re-reading 'Joy of clojure' along with doing this kata, I've stumbled upon the various ways to access vectors and treating a vector as an implicit map, which also noted the possibility to return a not-found value with `get` (as well as with `nth`). 

(defn chop
  [x vect]
  (loop [start 0
         end (count vect)]
    (let [middle (Math/round (Math/floor (/ (+ start end) 2)))
          mvalue (get vect middle -1)]
      (cond (> start end) -1
            (< x mvalue) (recur start (dec middle))
            (> x mvalue) (recur (inc middle) end)
            (= x mvalue) middle))))