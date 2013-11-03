(ns strmatch.logic.kmp-matcher
  (:use [strmatch.logic.common :only [discrepancy-index]]))

(defn- proper-heads [lst]
  (map #(take % lst)
       (range 1 (count lst))))

(defn- proper-tails [lst]
  (map #(drop % lst)
       (range 1 (count lst))))

(defn- prefix? [candidate string]
  (= candidate (take (count candidate) string)))

(defn- fail-value [needle suffix]
  (count
    (first (filter #(prefix? % needle)
                   (proper-tails suffix)))))

(defn failure-array
  [needle]
  (let [chs (seq needle)
        hds (proper-heads chs)]
    (vec (cons -1 
          (map #(fail-value needle %) hds)))))

(defn- highlight-for
  [offset discrep needle]
  (set (range offset
              (+ offset (or discrep (count needle))))))

(defn- entry-for
  [index discrep needle]
  {:index index
   :colors (vec 
             (concat (repeat index nil)
             (map (constantly :green) (highlight-for index discrep needle))
                     [:red]))})

(defn match
  [needle haystack]
  (let [fail (failure-array needle)]
    (loop [index 0
           acc []]
      (let [discrep (discrepancy-index needle haystack index)
            jump (- discrep (fail discrep))]
        (if (and discrep (<= index (count haystack)))
          (recur (+ index jump) (conj acc (entry-for index discrep needle)))
          (conj acc (entry-for index discrep needle)))))))
