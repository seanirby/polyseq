(ns polyseq.util
  (:require [om.core :as om]))

(def penta (mapv #(* 4 %) [54 64 72 81 96 108]))

(def piano (let [octave-multipliers (cons 1 (take 6 (iterate #(* % 2) 2)))
                 first-octave-keys (map vector
                                     [:white :black :white :white :black :white :black :white :white :black :white :black]
                                     [27.5 29.1353 30.8677 32.7032 34.6479 36.7081 38.8909 41.2035 43.6536 46.2493 48.9995 51.9130])]
             (sort-by :frequency (for [m octave-multipliers
                                       [c f] first-octave-keys]
                                   {:color c :frequency (* m f)}))))

(defn get-root-sub-cursor [app-state k]
  (om/ref-cursor (k (om/root-cursor app-state))))

(defn qualify-str [s qualifier]
  (str qualifier "-" s))

(defn px [s]
  (str s "px"))

(defn to-radians [degrees]
  (/ (* degrees Math/PI) 180))

(defn get-css-coordinates [degrees radius point-radius]
  "Calculates the CSS coordinates needed to correctly place a point on a circle"
  (let [r (to-radians degrees)
        x (px (+ (- point-radius) radius (* radius (Math/sin r))))
        y (px (+ (- point-radius) radius (* -1 radius (Math/cos r))))]
    [x y]))

(defn display [show]
  (if show
    #js {}
    #js {:display "none"}))

(defn current-time []
  (.now (.-performance js/window)))

(defn bpm-to-period [bpm]
  (/ 1 (/ bpm 60)))

(defn get-beat-period [n bpm]
  (/ (* (bpm-to-period bpm) 4) n))

(defn make-beat [beat]
  {:beat beat
   :sound-params nil
   :is-active false
   :is-playing false})

(defn make-circle [frequency]
  {:frequency frequency
   :radius (* frequency 20)
   :offset 0
   :beats (mapv make-beat (range 0 frequency 1))})

(defn make-circles [frequency]
  (mapv make-circle (range 1 (+ 1 frequency) 1)))

(defn point-style [angle circle-radius is-active]
  (let [color (if is-active "white" "#3F3F3F")
        point-radius 7
        [x y] (get-css-coordinates angle circle-radius point-radius)]
    #js {:left x
         :top y
         :width (px (* 2 point-radius)) 
         :height (px (* 2 point-radius))
         :borderRadius (px point-radius)
         :backgroundColor color}))

(defn circle-style [frequency radius offset]
  #js {:left (str "calc(50% - " (px radius) ")")
       :top (str "calc(50% - " (px radius) ")")
       :width (px (* 2 radius)) 
       :height (px (* 2 radius))
       :borderWidth (px 1) 
       :borderRadius (px radius)  
       :zIndex (- 100 frequency)})

(defn get-key-classes [color is-active]
  (let [base "key"
        color (qualify-str (name color) base)
        active (when is-active (qualify-str "active" base))]
    (apply str (interpose " " [base color active]))))

(defn beat-to-angle [beat frequency]
  (* beat (/ 360 frequency)))
