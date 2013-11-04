(ns strmatch.logic.common)

(defn discrepancy-index
  [needle haystack match-index]
  (let [nchars (seq needle)
        hchars 
        (take (count nchars) (drop match-index (seq haystack)))]
    (loop [index 0
           needle nchars
           haystack hchars]
      (cond (and (empty? needle) (empty? haystack)) false
            (= (first needle) (first haystack)) (recur (inc index) (rest needle) (rest haystack))
            :else index))))

(defn color-array [padding length-of-match]
  (vec (concat (repeat padding nil)
               (repeat length-of-match :green)
               (list :red))))
