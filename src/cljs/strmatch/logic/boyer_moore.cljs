(ns strmatch.logic.boyer-moore
  (:use [strmatch.logic.common :only [discrepancy-index]]))

(defn last-occurrence
  [string]
  (fn [chr]
    (.lastIndexOf string chr)))

(defn- matches-at
  [string suffix index]
  (let [chrs (vec string)]
    (and
      (or (nil? (chrs index))
          (not (= (chrs index) (suffix 0))))
      (every? (fn [i]
                (or (nil? (chrs (+ i index)))
                    (= (chrs (+ i index)) (suffix i))))
              (range 1 (count suffix))))))

(defn- match-index
  [string suffix]
  (second
    (first 
      (filter first
              (map #(vector (matches-at string (vec suffix) %) %)
                   (range (- (count string) (count suffix)) (- -1 (count suffix)) -1))))))

(defn- bad-suffix-for-index
  [string index]
  (let [suffix (drop index string)]
    (match-index string suffix)))

(defn bad-suffix
  [string]
  (vec (map 
  #(bad-suffix-for-index string %)
    (range 0 (count string)))))

(defn- reverse-discrepancy-index
  [needle haystack index]
  (let [relevant-haystack (take (count needle) (drop index haystack))
        reversed-index (discrepancy-index (reverse needle) (reverse relevant-haystack) 0)]
    (- (count needle) (if reversed-index (inc reversed-index) (count needle)))))

(defn- calculate-jump
  [needle haystack index]
  (let [discrep (reverse-discrepancy-index needle haystack index)
        bad-suff (bad-suffix needle)
        last-occ (last-occurrence needle)]
    (max
      (- discrep (bad-suff discrep))
      (- discrep (last-occ (nth haystack (+ index discrep)))))
    ))

(defn match
  ([needle haystack] (match needle haystack 0))
  ([needle haystack index]
  (let [discrep (reverse-discrepancy-index needle haystack index)
        jump (calculate-jump needle haystack index)]
    (prn discrep)
    (cons
      {
       :index index 
       :colors (concat (repeat (+ index discrep) nil) (if (zero? discrep) [] [:red]) (repeat (- (count needle) discrep) :green) )
       }
    (if (or (= discrep 0) (> (+ index discrep) (count haystack)))
      '()
      (match needle haystack (+ index jump)))))))
