(ns strmatch.ui.core
  (:require [strmatch.logic.kmp-matcher]
            [strmatch.logic.brute-force]
            [strmatch.logic.boyer-moore])
  (:use [jayq.core :only [$ val css html append delegate]]))


(def $go ($ :#go))
(def $needle ($ :#needle))
(def $haystack ($ :#haystack))

(delegate $go "" :click
       (fn [e]
         (let [needle (val $needle)
               haystack (val $haystack)]
           (show-match needle haystack))))
   
(defn match-fn
  []
  ({"naive"  strmatch.logic.brute-force/match
    "kmp" strmatch.logic.kmp-matcher/match
    "boyer_moore" strmatch.logic.boyer-moore/match
    }
   (.-value (first ($ ".algbutton:checked")))))

(defn set-dimens
  [w h]
  (html ($ :#display)
        (str
          (map (fn [y]
                 (str "<tr>"
                 (map (fn [x]
                        (str "<td class='cell' id='display" x "x" y "'></td>"))
                      (range 0 w))
                      "</tr>"))
               (range 0 h)))))

(def color-for
  {:green "81F13D"
   :red "FF0000"})

(defn set-cell
  [x y value color]
  (let [el ($ (str "#display" x "x" y))]
    (html el value)
    (when color
      (css el "background-color" (color-for color)))))

(defn set-row
  [row value colors]
  (let [cs (vec (seq value))]
    (doseq [x (range 0 (count cs))]
      (set-cell x row (cs x) (colors x)))))

(defn show-match
  [needle haystack]
  (let [match-result ((match-fn) needle haystack)
        results match-result]
    (set-dimens (count haystack) (inc (count results)))
    (set-row 0 haystack #{})
    (doall
      (map-indexed
        (fn [i result]
          (set-row (inc i)
                   (str (apply str (repeat (:index result) " ")) needle)
                   (vec (:colors result))))
        results))))


