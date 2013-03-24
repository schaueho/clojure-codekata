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


 


 
