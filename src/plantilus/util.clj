(ns plantilus.util)

;; from miner.halfbaked
(defn fs-join [& path-elements]
  "Joins path-elements into a string using the File/separator between elemements."
  (apply str (interpose java.io.File/separator path-elements)))

;; seems generally useful
;; might be better with a reduce
(defn map-by [fkey fval coll]
  (into {} (map (juxt fkey fval) coll)))

(defn mapmap [f mp]
  (persistent!
   (reduce-kv (fn [tm k v] (assoc! tm k (f v)))
              (transient mp)
              mp)))

(defn keep-duplicates [f coll]
  [coll]
  (loop [seen #{} dups #{} coll (keep f coll)]
    (if-let [cs (seq coll)]
      (let [item (first cs)]
        (if (contains? seen item)
          (recur seen (conj dups item) (rest cs))
          (recur (conj seen item) dups (rest cs))))
      (not-empty dups))))


;; Like standard interleave but doesn't drop excess elements; also works with zero or one
;; arg.  Of course, this should not be used with infinite sequences.
(defn interleave-all
  "Returns a lazy seq of the first item in each collection, then the second, etc.  If one 
collection ends, continues to interleave the others.  Of course, this should not be used
with infinite sequences."
  ([] nil)
  ([c] (lazy-seq c))

  ([c1 c2]
     (lazy-seq
      (let [s1 (seq c1) s2 (seq c2)]
        (if (and s1 s2)
          (cons (first s1) (cons (first s2) 
                                 (interleave-all (rest s1) (rest s2))))
	  (or s1 s2)))))

  ([c1 c2 & colls] 
     (lazy-seq 
      (let [ss (map seq (conj colls c2 c1))]
        (if (every? identity ss)
          (concat (map first ss) (apply interleave-all (map rest ss)))
	  (apply interleave-all (filter identity ss)))))))
