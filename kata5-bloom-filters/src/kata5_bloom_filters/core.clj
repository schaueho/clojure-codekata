(ns kata5-bloom-filters.core)

; cf. http://www.cse.yorku.ca/~oz/hash.html
(defn sum-chars 
  "Sum up the chars of a given string"
  ; this is essentially K&R "lose-lose" pretty bad hashing algorithm
  [charseq]
  (reduce + (map int charseq)))

(defn djb-string-hash
  "Use djb's method for hashing a string"
  [charseq]
  (reduce (fn [curhash charval]
            ^long (unchecked-add 
                   (unchecked-add (bit-shift-left curhash 5) curhash) 
                   charval))
          (cons 5381 (map int charseq))))

(defn- comp-hash-val
  [curhash charval]
  (unchecked-subtract 
   ^long (unchecked-add
          charval
          (unchecked-add (bit-shift-left curhash 16) (bit-shift-left curhash 6)))
   (unchecked-long curhash)))

(defn sdbm-string-hash
  "Use the method from sdbm for hashing a string"
  [charseq]
  (reduce comp-hash-val
          (cons  0 (map int charseq))))

(defn sdbm-hash-recur 
  "Recursive version of the sdbm hash"
  ;; Compute hash(i) = hash(i - 1) * 65599 + str[i]; 
  ([charseq] (sdbm-hash-recur charseq 0))
  ([charseq ^long acc]
     (if (empty? charseq)
       acc
       (recur (rest charseq) 
              (unchecked-add (unchecked-multiply acc 65599) 
                             (int (first charseq)))))))
  
; cf. http://www.isthe.com/chongo/tech/comp/fnv/
(defn fnv-hash [charseq]
  (let [fnv-prime 0x811C9DC5]
    (reduce (fn [^long curhash charval]
              ^long (bit-xor (unchecked-multiply curhash fnv-prime) charval))
            (cons 0 (map (comp unchecked-long int) charseq)))))