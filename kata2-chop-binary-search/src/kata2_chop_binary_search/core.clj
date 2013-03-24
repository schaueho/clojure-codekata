(ns kata2-chop-binary-search.core)

(defn chop-keep-indexed [x coll]
  (let [result
        (keep-indexed
         (fn [idx item]
           (when (= item x) idx))
         coll)]
    (if (empty? result)
      -1
      (first result))))

(defn chop [x coll]
  (let [result (keep-indexed #(when (= %2 x) %1) coll)]
    (or (first result) -1)))

