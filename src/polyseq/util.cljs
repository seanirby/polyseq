(ns polyseq.util)

(def penta (mapv #(* 4 %) [54 64 72 81 96 108]))

(def piano (let [octave-multipliers (cons 1 (take 6 (iterate #(* % 2) 2)))
                 first-octave-keys (map vector
                                     [:white :black :white :white :black :white :black :white :white :black :white :black]
                                     [27.5 29.1353 30.8677 32.7032 34.6479 36.7081 38.8909 41.2035 43.6536 46.2493 48.9995 51.9130])]
             (sort-by :frequency (for [m octave-multipliers
                                       [c f] first-octave-keys]
                                   {:color c :frequency (* m f)}))))

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

(defn make-beat-data [beat]
  {:beat beat
   :frequency (first (shuffle [110 220 440 880]))
   :is-active (first (shuffle [true false]))
   :is-playing false})

(defn make-circle-data [frequency]
  {:frequency frequency
   :radius (* frequency 20)
   :offset 0
   :beats (mapv make-beat-data (range 0 frequency 1))})

(defn point-style [angle radius is-active]
  (let [color (if is-active "white" "#3F3F3F")
        point-radius 7
        [x y] (get-css-coordinates angle radius point-radius)]
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
