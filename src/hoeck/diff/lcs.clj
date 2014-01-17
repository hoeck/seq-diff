;   Copyright (c) 2011, Erik Soehnel All rights reserved.
;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

;;; longest common subsequence implementation as described in:
;;; http://en.wikipedia.org/wiki/Longest_common_subsequence_problem

(ns hoeck.diff.lcs)

(defn lcs-matrix
  "Builds and fills a matrix to find the longest common subsequence
  (LCS) of two sequences s and t.
  Both must be Indexed (supporting the nth method).
  Returns a twodimensional array of Integers"
  [s t & {:keys [compare] :or {compare =}}]
  (let [rows (inc (count s))
        cols (inc (count t))
        a (make-array Long rows cols )]
    (doseq [r (range rows)] (aset a r 0 0))
    (doseq [c (range cols)] (aset a 0 c 0))
    (doseq [i (range 1 rows)]
      (doseq [j (range 1 cols)]
        (if (compare (nth s (dec i)) (nth t (dec j)))
          (aset a i j (inc (aget a (dec i) (dec j))))
          (aset a i j (max (aget a i (dec j))
                           (aget a (dec i) j))))))
    a))

(defn lcs-vec
  "Given a lcs-matrix (created with the lcs-matrix function) and the
  corresponding sequences, returns the actual lcs as a vector."
  [lcs-matrix s t & {:keys [compare] :or {compare =}}]
  (let [m lcs-matrix
        f (fn lcs-vec-rec [i j]
            (cond (or (= 0 i) (= 0 j))
                  []
                  (compare (nth s (dec i)) (nth t (dec j)))
                  (conj (lcs-vec-rec (dec i) (dec j)) (nth s (dec i)))
                  (< (aget m i (dec j)) (aget m (dec i) j))
                  (lcs-vec-rec (dec i) j)
                  :else
                  (lcs-vec-rec i (dec j))))]
    (f (count s) (count t))))

(defn lcs-diff
  "Given a lcs-matrix (created with lcs-matrix) and the corresponding
  sequences, returns a vector containing two element vectors of
  [operation element], where operation is one of nil, :old or :new.
  Options are:
    :compare .. a function to compare two elements, defaults to ="
  [lcs-matrix s t & {:keys [compare] :or {compare =}}]
  (let [m lcs-matrix
        f (fn lcs-diff-rec [i j]
            (cond (and (< 0 i) (< 0 j) (compare (nth s (dec i)) (nth t (dec j))))
                  (conj (lcs-diff-rec (dec i) (dec j)) [nil (nth s (dec i))])
                  (and (< 0 j) (or (= i 0) (<= (aget m (dec i) j) (aget m i (dec j)))))
                  (conj (lcs-diff-rec i (dec j)) [:new (nth t (dec j))])
                  (and (< 0 i) (or (= j 0) (>  (aget m (dec i) j) (aget m i (dec j)))))
                  (conj (lcs-diff-rec (dec i) j) [:old (nth s (dec i))])
                  :else []))]
    (f (count s) (count t))))

(defn vec-diff
  "Create a diff of two given vectors. Return a vector of [op element]
  where op may be one of nil, :removed or :inserted.
  Options are:
    :compare .. a function to compare two elements, defaults to =
                in each call to compare, an element of vector v is
                compared to an element of vector w."
  [v w & opts]
  (apply lcs-diff (apply lcs-matrix v w opts) v w opts))

;; (time (vec-diff (vec (concat (repeat 100 :a) [:x :c])) (vec (concat (repeat 100 :a) [:x :c :a :a]))))
;; (vec-diff [:x :c :a :b] [:a :x :c])

