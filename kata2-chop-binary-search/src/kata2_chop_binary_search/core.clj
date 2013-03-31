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

(defn chop-keep-indexed-short [x coll]
  (let [result (keep-indexed #(when (= %2 x) %1) coll)]
    (or (first result) -1)))

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
(defn chop-multi [x coll]
  (chop-helper x coll 0))


(defn halve [coll comp]
       (let [len (count coll)
              half (/ len 2)]
         (keep-indexed #(if (comp %1 half) %2) coll)))

(defn chop [x coll]
  (let [len (count coll)
        mididx (Math/round (Math/ceil (/ len 2)))
        first-half (halve coll <)
        second-half (halve coll >)
        half-value (last first-half)]
    (cond (= x half-value) (- mididx 1)
          (and (< x half-value)
               (not (empty? first-half)))
            (chop x first-half)
          (and (> x half-value)
               (not (empty? second-half)))
            (let [res-sh (chop x second-half)]
              (if (= res-sh -1)
                -1
                (+ mididx res-sh)))
          true -1)))