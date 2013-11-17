(ns strmatch.ui.core
  (:require [strmatch.logic.kmp-matcher]
            [strmatch.logic.brute-force]
            [strmatch.logic.boyer-moore]
            [cljs.core.async :refer [put! chan <! close!]])
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:use [jayq.core :only [$ val css html append delegate anim dequeue]]
        [jayq.util :only [log]])
  (:use-macros [jayq.macros :only [queue]]))

(def jump-duration 250)
(def wait-after-jump 250)
(def wait-after-fade 500)
(def fade-duration 200)
(def wait-before-jump 500)

(def $animate ($ :#animate))
(def $step ($ :#step))
(def $reset ($ :#reset))
(def $needle-input ($ :#needle-input))
(def $haystack-input ($ :#haystack-input))

(def *playback-data* {})
(def *playback-index* 0)

(def step-channel (chan))

(defn match-fn []
  ({"naive"  strmatch.logic.brute-force/match
    "kmp" strmatch.logic.kmp-matcher/match
    "boyer_moore" strmatch.logic.boyer-moore/match}
   (.-value (first ($ ".algbutton:checked")))))

(def color-for
  {:green "#81F13D"
   :red "#FF0000"
   :white "#FFFFFF"
   :yellow "#FFFF00"})

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

(defn queue-step
  [step]
  (let [$elem ($ :#needle)
        index (:index step)
        colors (:colors step)
        explanation (:explanation step)]
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
    (.delay $elem wait-before-jump)))

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

(defn reset-playback []
  (let [needle (val $needle-input)
        haystack (val $haystack-input)
        match-result ((match-fn) needle haystack)
        match-animation (:animation match-result)
        match-tables (:tables match-result)]
    (show-tables match-tables)
    (html ($ :#explanation) "")
    (html ($ :.result)
          (str 
            "<div id=\"haystack\" class=\"monospace\"></div></br>"
            "<div id=\"needle\" class=\"monospace\"></div>"))
    (set-value-divs ($ :#haystack) haystack)
    (set-value-divs ($ :#needle) needle)
    (css ($ :#needle) {:left (position-of-haystack)})
    (set! *playback-index* 0)
    (set! *playback-data* match-animation)))

(doseq [radio ($ :.algbutton)]
  (delegate ($ radio) "" :click reset-playback))

(doseq [input [$needle-input $haystack-input]]
  (delegate input "" :keyup reset-playback))

(delegate $step "" :click
          (fn [e]
            (when (< *playback-index* (count *playback-data*))
              (put! step-channel *playback-index*)
              (set! *playback-index* (inc *playback-index*)))))

(delegate $animate "" :click
          (fn [e]
            (let [max-val (count *playback-data*)]
              (doseq [i (range *playback-index* max-val)]
                (put! step-channel i))
              (set! *playback-index* max-val))))

(delegate $reset "" :click reset-playback)

(go (while true
      (let [index (<! step-channel)]
        (queue-step (nth *playback-data* index)))))

(reset-playback)
