(ns kata2-chop-binary-search.core)

(defn chop-keep-indexed
  "Simple search using a lazy sequence"
  [x coll]
  (let [result
        (keep-indexed
         (fn [idx item]
           (when (= item x) idx))
         coll)]
    (if (empty? result)
      -1
      (first result))))

(defn chop-keep-indexed-short
  "Simple search using a lazy sequence, more idiomic"
  [x coll]
  (let [result (keep-indexed #(when (= %2 x) %1) coll)]
    (or (first result) -1)))

(defn chop-iterate-for 
  [x coll]
  "Simple search, as iteratively as possible"
  (or (first (for [val-idx (map vector coll (iterate inc 0))
                   :let [[val idx] val-idx]
                   :when (= val x)]
               idx))
      -1))

(derive clojure.lang.Sequential ::collection)
(defmulti chop-helper
  "Dispatch function returning the empty collection or the type of the collection"
  (fn [x coll idx]
    (if (empty? coll) coll
        (type coll))))
(defmethod chop-helper [] [x coll idx]  -1)
(defmethod chop-helper ::collection [x coll idx]
  (if (= x (first coll))
    idx
    (chop-helper x (rest coll) (inc idx))))
(defmethod chop-helper :default [x coll] :oops)  
(defn chop-multi
  "Multi-method recursive approach to linear search"
  [x coll]
  (chop-helper x coll 0))


(defn chop-bin-vec-optarg
  "Recursive binary search of a vector with optional argument destructuring for accumulator"
  [x vec & {:keys [acc] :or {acc 0}}]
  (let [len (count vec)
        middle (Math/round (Math/floor (/ len 2)))]
    ; (println x vec acc :middle middle)
    (cond (empty? vec) -1
          (= x (nth vec middle)) (+ acc middle)
          (and (< x (nth vec middle))
               (>= middle 0))
             (recur x (subvec vec 0 middle) {:acc acc})
          (and (> x (nth vec middle))
               (<= (inc middle) len))
             (recur x (subvec vec (inc middle) len) {:acc (inc middle)})))) 

(defn chop-bin-rec
  "Recursive binary search of a vector"
  ([x vec]
     (chop-bin-rec x vec 0))
  ([x vec acc]
     (let [len (count vec)
           middle (Math/round (Math/floor (/ len 2)))]
       ;; (println x vec acc :middle middle)
       (cond (empty? vec) -1
             (= x (nth vec middle))
               (+ acc middle)
             (and (< x (nth vec middle))
                  (>= middle 0))
               (recur x (subvec vec 0 middle) acc)
             (and (> x (nth vec middle))
                  (<= (inc middle) len))
             (recur x (subvec vec (inc middle) len) (inc middle))))))

(defn chop
  "Iterative binary search of a vector"
  [x vect]
  (loop [start 0
         end (count vect)]
    (let [middle (Math/round (Math/floor (/ (+ start end) 2)))
          mvalue (get vect middle -1)]
      ;; (println :x x :start start :end end :middle middle :mvalue mvalue)
      (cond (> start end) -1
            (< x mvalue) (recur start (dec middle))
            (> x mvalue) (recur (inc middle) end)
            (= x mvalue) middle))))
      
            