(ns strmatch.logic.brute-force
  (:use [strmatch.logic.common :only [discrepancy-index]]))

(defn- colors-for
  [needle haystack index]
  (vec (let [discrep (discrepancy-index needle haystack index)]
    (concat 
      (repeat index nil)
      (repeat (or discrep (count needle)) :green)
            [:red]))))

(defn match
  ([needle haystack] (vec (match needle haystack 0)))
  ([needle haystack index]
  (let [discrepancy (discrepancy-index needle haystack index)
        colors (colors-for needle haystack index)]
    (if (and discrepancy (<= index (count haystack)))
      (cons {:index index :colors colors}
            (match needle haystack (inc index)))
      [{:index index :colors colors}]))))
