(ns strmatch.logic.kmp-matcher
  (:use [strmatch.logic.common :only [discrepancy-index color-array]]))

; Implementation of the KMP algorithm for string matching.
; http://en.wikipedia.org/wiki/Knuth%E2%80%93Morris%E2%80%93Pratt_algorithm

(defn- proper-prefixes [lst]
  (map #(take % lst)
       (range 1 (count lst))))

(defn- proper-suffixes [lst]
  (map #(drop % lst)
       (range 1 (count lst))))

(defn- prefix? [candidate string]
  (= candidate (take (count candidate) string)))

(defn- fail-value [needle suffix]
  (count (first (filter #(prefix? % needle)
                        (proper-suffixes suffix)))))

; Calculates the failure array.
; The value at every index i is the length of the 
; longest proper suffix of `(take i needle)` which is
; also a prefix of `needle`.
; For convenience, the first value is always -1.
(defn failure-array [needle]
  (let [chars (seq needle)
        heads (proper-prefixes chars)]
    (vec (cons -1 (map #(fail-value needle %) heads)))))

(defn- entry-for [index discrep needle already-matched]
  (let [to-ignore (max 0 already-matched)
        padding (+ to-ignore index)
        length-of-match (if discrep (- discrep to-ignore) (count needle))]
    {:index index
     :colors (color-array padding length-of-match)}))

(defn match
  ([needle haystack] (vec (reverse (match needle haystack (failure-array needle) 0 nil 0))))
  ([needle haystack fail-array index result prev-fail]
   (let [discrep (discrepancy-index needle haystack index)
         jump (- discrep (fail-array discrep))
         next-entry (entry-for index discrep needle prev-fail)
         next-result (cons next-entry result)]
     (if (and discrep (<= index (count haystack)))
       (recur needle haystack fail-array (+ index jump) next-result (fail-array discrep))
       next-result))))
