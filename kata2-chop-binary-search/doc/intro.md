# Introduction to kata2-chop-binary-search

I started out with the simplest approach I could think of: there just
might be a function already readily available in Clojure which solves
the problem. After all, Common Lisp has 'position' whereas Pythonistas
would use the 'index' method on lists.

I couldn't really find a function on clojure sequences which would
immediately take care of the issue, but 'keep-indexed' seems close
enough -- taking a function and a collection, it calls the function
which ought to take an index and a value, and keeps the function's
returned non-nil values. This led to the following code:

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
- Looking over some code, I ran into the usage of %1 etc. to refer to
  implicit arguments, which I didn't knew about when I started
  out. This version screams 'Common Lisp' pretty much all over.
- I also ran into the empty sequence <> false issue, of course and had
  to look up 'empty?' as well.
- Using 'when' instead of 'if' when you only care about the true state
  is an idiom I knew and love from CL. The 'if' part at the end is 
  still ugly. We could get rid of it by relying on 'first' on an empty
  sequence to return nil and using this in a boolean comparison.

This leads to the following much shorter version:

(defn chop [x coll]
  (let [result (keep-indexed #(when (= %2 x) %1) coll)]
    (or (first result) -1)))


 


 
