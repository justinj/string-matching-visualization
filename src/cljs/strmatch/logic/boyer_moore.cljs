(ns strmatch.logic.boyer-moore
  (:use [strmatch.logic.common :only [discrepancy-index]]))

; The Boyer-Moore algorithm has a couple differences from KMP.
; First, it begins trying to match from the right side, rather than the left.
; Second, it uses two tables to calculate its jumps (last occurrence and suffix), rather than just one.
; In any situation, it checks what jump either table would allow, and takes the better one.
; It happens that the two tables tend to complement each other quite well, so large jumps are often possible.
;
; https://en.wikipedia.org/wiki/Boyer%E2%80%93Moore_string_search_algorithm

; The last-occurrence checker takes a character and returns the last occurence
; of that character in the needle, or -1 if it does not occur.
; This is used with the haystack to push the needle past all the points where the current
; haystack character could not possibly occur.
; Example:
;
; needle: jey
; haystack: abjjey
; -> abjjey
; -> jey
;
; First, we check the value at i = 2 for a match.
; We don't get one. Then we find the "last occurrence" of 'j' in our needle, and align that with the 'j':
;
; -> abjjey
; ->   jey
;
; The same process repeats with the 'e', we align the 'e' with the last occurence:
; -> abjjey
; ->    jey
;
; And find the match.

(defn last-occurrence
  [string]
  (fn [chr]
    (.lastIndexOf string chr)))

(defn- matches-at
  [string suffix index]
  (let [chrs (vec string)]
    (and
      (or (neg? index)
          (not (= (chrs index) (suffix 0))))
      (every? (fn [i]
                (or (neg? (+ i index))
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

; The suffix checker is constructed as follows:
;
; For index `i`, take `s`, the substring beginning at index `i`.
; so if our string is NEEDLE, and i = 4, we take `s` = "LE".
; We then find the farthest right alignment of `s` with the needle,
; such that the first character *does not* match, and the rest of `s` does.
;
; In this case, the alignment is:
; -> NEEDLE
; ->  LE
;
; You could think of it as finding the largest index that [^L]E matches.

(defn bad-suffix
  [string]
  (vec (map 
  #(bad-suffix-for-index string %)
    (range 0 (count string)))))

(defn- reverse-discrepancy-index
  [needle haystack index]
  (let [relevant-haystack (take (count needle) (drop index haystack))
        reversed-index (discrepancy-index (reverse needle) (reverse relevant-haystack) 0)]
    (if reversed-index
    (- (count needle) (inc reversed-index))
      false)))

(defn- calculate-jump
  [needle haystack index]
  (let [discrep (reverse-discrepancy-index needle haystack index)
        bad-suff (bad-suffix needle)
        last-occ (last-occurrence needle)]
    (- discrep (min (bad-suff discrep)
                    (last-occ (nth haystack (+ index discrep))))))) 

(defn- color-array [index discrep needle]
  (concat (map (fn [i] { :color :green
                       :index i })
              (reverse (range (if discrep (inc discrep) 0) (count needle))))
          (if discrep [{:index discrep :color :red}] [])))

(defn- explanation-for
  [needle haystack index]
  (let [discrep (reverse-discrepancy-index needle haystack index)
        bad-suff (bad-suffix needle)
        bad-suff-value (bad-suff discrep)
        last-occ (last-occurrence needle)
        haystack-char (nth haystack (+ index discrep))
        last-occ-value (last-occ haystack-char)]
    (cond
      (nil? haystack-char) "No match found :("
      discrep (str
                "discrepancy_index = " discrep "<br>"
                "last_occurrence(" haystack-char ") = " last-occ-value "<br>"
                "bad_suffix(" discrep ") = " bad-suff-value
                "<br><br>"
                "Bad Suffix gives a jump of (" discrep ") - (" bad-suff-value ") = " (- discrep bad-suff-value) "<br>"
                "Last Occurrence gives a jump of (" discrep ") - (" last-occ-value") = " (- discrep last-occ-value) "<br>"
                "So we "
                (cond (< bad-suff-value last-occ-value) "go with Bad Suffix"
                      (> bad-suff-value last-occ-value) "go with Last Occurrence"
                      :else "are indifferent"))
      :else "Match found!")))

(defn- match-data
  [needle haystack]
  (loop [index 0
         acc []]
    (let [discrep (reverse-discrepancy-index needle haystack index)
          jump (calculate-jump needle haystack index)
          colors (color-array index discrep needle)
          explanation (explanation-for needle haystack index)
          entry {:index index
                 :colors colors
                 :explanation explanation}
          result (conj acc entry)]
      (if (and discrep
               (<= (+ index (count needle)) (count haystack)))
        (recur (+ index jump) result)
        result))))

(defn match
  [needle haystack]
  (let [bad-suff (bad-suffix needle)
        last-occ (last-occurrence needle)]
    {:animation (match-data needle haystack)
     :tables [(concat [["i" "Suffix Location"]] (map vector (range) bad-suff))
              (concat [["char" "Last Occurrence"]]
                      (map #(vector % (last-occ %)) (distinct needle)))]}))
