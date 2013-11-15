(ns strmatch.ui.core
  (:require [strmatch.logic.kmp-matcher]
            [strmatch.logic.brute-force]
            [strmatch.logic.boyer-moore])
  (:use [jayq.core :only [$ val css html append delegate anim dequeue]]
        [jayq.util :only [log]])
  (:use-macros [jayq.macros :only [queue]]))

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
  {:green "#81F13D"
   :red "#FF0000"
   :white "#FFFFFF"})

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

(def div-width 30)

(defn set-value-divs 
  [$elem value]
  (.empty $elem)
  (doseq [[i char] (map-indexed vector value)]
    (append $elem 
            (str "<div "
                 "style='width:" div-width "px;float:left'"
                 "id='cell" i "'"
                 "class='cell'"
                 ">" (if (= char \space) "&nbsp;" char)
                 "</div>"))))

(defn color [$elem col index]
  (let [actual-color (color-for col)]
    (queue $elem
           (anim (.children $elem (str "#cell" index))
                 {:background-color actual-color}
                 fade-duration)
           (dequeue $elem))
    $elem))

(defn- position-of-haystack []
  (-> :#haystack $ .position .-left))

(def jump-duration 250)
(def wait-after-jump 250)
(def wait-after-fade 500)
(def fade-duration 200)
(def wait-before-jump 500)

(defn animate-match
  [match-result $elem]
  (loop [results match-result]
    (if (empty? results)
      $elem
      (let [index (:index (first results))
            colors (:colors (first results))
            explanation (:explanation (first results))]
        (queue $elem
               (-> $elem
                   .children
                   (css :background-color "#FFFFFF"))
               (dequeue $elem))
        (-> $elem
            (anim {:left 
                   (+ (position-of-haystack) (* div-width index))} jump-duration)
            (.delay wait-after-jump))
        (doseq [color-event colors]
          (-> $elem
              (color (:color color-event)
                     (:index color-event))
              (.delay wait-after-fade)))
        (queue $elem
               (html ($ :#explanation) explanation)
               (dequeue $elem))
        (.delay $elem wait-before-jump)
        (recur (rest results))))))

(defn table-html [table]
  (str "<table class=''>"
       "<col width='60px' />"
       "<col width='60px' />"
       (clojure.string/join
         (map (fn [entries]
                (str "<tr>"
                     (clojure.string/join
                       (map #(str "<td class='table-cell'>" % "</td>")
                            entries))))
              table))
       "</table>"))

(defn show-table [table]
  (append ($ :#tables)
        (table-html table)))

(defn show-tables [tables]
  (html ($ :#tables) "")
  (doseq [table tables]
      (show-table table)))

(defn show-match
  [needle haystack]
  (let [match-result ((match-fn) needle haystack)
        match-animation (:animation match-result)
        match-tables (:tables match-result)]
    (show-tables match-tables)
    (html ($ :.result)
          (str 
            "<div id=\"haystack\" class=\"monospace\"></div></br>"
            "<div id=\"needle\" class=\"monospace\"></div>"))
    (set-value-divs ($ :#haystack) haystack)
    (set-value-divs ($ :#needle) needle)
    (css ($ :#needle) {:left (position-of-haystack)})
    (set! *playback-data* {:match-data match-animation :index 0})
    (animate-match match-animation ($ :#needle))))

(def *playback-data*
  {})

(def *playing?* false)

(def $go ($ :#go))
(def $needle-input ($ :#needle-input))
(def $haystack-input ($ :#haystack-input))


(delegate $go "" :click
          (fn [e]
            (let [needle (val $needle-input)
                  haystack (val $haystack-input)]
              (show-match needle haystack))))
