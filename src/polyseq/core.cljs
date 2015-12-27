(ns polyseq.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [polyseq.util :as util]
            [cljs.core.async :refer [timeout mult tap untap put! chan <! >!]]
            [clojure.string :as string]))



(enable-console-print!)

(defn make-beat-data [beat]
  {:beat beat
   :frequency (first (shuffle util/penta))
   :is-active (first (shuffle [true false]))
   :is-playing false})

(defn make-circle-data [frequency]
  {:frequency frequency
   :radius (* frequency 30)
   :offset 0
   :beats (mapv make-beat-data (range 0 frequency 1))})

(def angle-chan (chan))
(def angle-mult (mult angle-chan))
(def num-circles 10)
(def circles-range (range 1 (+ 1 num-circles) 1)) ;;value of circle here is the # of beats in a circle AKA the frequency
(defonce app-state 
  (atom {:controls {:bpm 60}
         :circles (mapv make-circle-data circles-range)
         :tooltip {:display false
                   :frequency nil}}))

(defn point-style [angle radius is-active]
  (let [color (if is-active "white" "#3F3F3F")
        point-radius 10
        [x y] (util/get-css-coordinates angle radius point-radius)]
    #js {:left x
         :top y
         :width (util/px (* 2 point-radius)) 
         :height (util/px (* 2 point-radius))
         :borderRadius (util/px point-radius)
         :backgroundColor color}))

(defn circle-style [frequency radius offset]
  #js {:left (str "calc(50% - " (util/px radius) ")")
       :top (str "calc(50% - " (util/px radius) ")")
       :width (util/px (* 2 radius)) 
       :height (util/px (* 2 radius))
       :borderWidth (util/px 1) 
       :borderRadius (util/px radius)  
       :zIndex (- 100 frequency)})

(defn tooltip-view [data owner]
  (reify
    om/IRender
    (render [_]
      (when (:display data)
        (dom/div #js {:className "tooltip"}
          (dom/div #js {:className "key-container"}
            (vec (for [{:keys [color frequency]} util/piano
                       active (= frequency (:frequency data))]
                   (dom/div #js {:className (if (= color :white) "key key-white" "key key-black")})))))))))

(defn get-sound-wave [frequency]
  (js/T "pulse" #js {:freq frequency :mul 0.1}))

(defn get-sound-enveloped [wave release]
  (js/T "perc" #js {:r release} wave))

(defn point-view [{:keys [beat frequency is-active is-playing circle-frequency circle-radius] :as data} owner]
  (reify
    om/IInitState
    (init-state [_]
      {:angle-chan (tap (:angle-mult (om/get-shared owner)) (chan))})
    om/IWillMount
    (will-mount [_]
      (let [on-end #(this-as this (.pause this) (om/update! data :is-playing false))
            sound (.on (get-sound-enveloped (get-sound-wave frequency) 600)  "ended" on-end)
            angle-chan (:angle-chan (om/get-state owner))]
        (go-loop [angle (<! angle-chan)]
          (when (and
                  (not (:is-playing @data))
                  (:is-active @data)
                  (<= (- angle 20) (* 360 (/ beat circle-frequency)) angle))
            (.play (.bang sound))
            (om/update! data :is-playing true))
          (recur (<! angle-chan)))))
    om/IWillUnmount
    (will-unmount [_]
      (untap (:angle-mult (om/get-shared owner)) (:angle-chan (om/get-state owner))))
    om/IRender
    (render [_]
      (let [tooltip (om/ref-cursor (:tooltip (om/root-cursor app-state)))
            clazz (if is-playing "point playing" "point")
            style (point-style (* beat (/ 360 circle-frequency)) circle-radius is-active)]
        (dom/div #js {:className clazz
                      :style style
                      :onClick #(om/update! data :is-active (not is-active))
                      ;; :onMouseOver #(om/update! tooltip {:frequency (:freqency state)
                      ;;                                    :is-active (:is-active state)
                      ;;                                    :display true
                      ;;                                    :freq (:freq data)
                      ;;                                    })
                      ;; :onMouseOut #(om/update! tooltip {:display false})
                      })))))



(defn circle-view [{:keys [frequency radius offset beats] :as data} owner]
  (reify
    om/IRender
    (render [_]
      (let [style (circle-style frequency radius offset)]
        (dom/div #js {:style style :className "circle"}
          (om/build-all point-view (mapv #(assoc % :circle-frequency frequency :circle-radius radius) beats)))))))

(defn hand-view [_ owner]
  (reify
    om/IInitState
    (init-state [_]
      {:angle 0
       :angle-chan (tap (:angle-mult (om/get-shared owner)) (chan))})
    om/IWillMount
    (will-mount [_]
      (let [angle-chan (:angle-chan (om/get-state owner))]
        (go-loop [angle (<! angle-chan)]
          (om/set-state! owner :angle angle)
          (recur (<! angle-chan)))))
    om/IWillUnmount
    (will-unmount [_]
      (untap (:angle-mult (om/get-shared owner)) (:angle-chan (om/get-state owner))))
    om/IRenderState
    (render-state [_ state]
      (let [style #js {:transform (str "rotate(" (:angle state) "deg)")}]
        (dom/div #js {:className "hand"
                      :style style}
          (dom/div #js {:className "hand-top"}))))))



(defn controls-view [data owner]
  (reify
    om/IRender
    (render [_]
      (dom/div nil
        [(dom/button #js {} "Clear Sequencer")
         (dom/label nil "Speed:")
         (dom/input #js {:type "range"
                         :max 400
                         :min 20
                         :value (:bpm data)
                         :onChange #(om/update! data :bpm (.. % -target -value))})]))))



(defn circles-view [{:keys [circles tooltip bpm] :as data} owner]
  (reify
    om/IRender
    (render [_]
      (apply dom/div nil [(om/build-all circle-view circles)
                          [(om/build hand-view nil)
                           (om/build tooltip-view tooltip)
                           (om/build controls-view (:controls data))]]))))

(om/root circles-view
  app-state
  {:target (.getElementById js/document "app")
   :shared {:angle-mult angle-mult}})

(defonce angle-trigger ((fn []
                          (go-loop [acc 0
                                    bpm (:bpm (:controls @app-state))
                                    t (util/current-time)
                                    _ (<! (timeout 10))]
                            (let [elapsed (/ (- (util/current-time) t) 1000)
                                  acc' (mod (+ acc (* bpm 0.02)) 360)] ;;TODO get the actual formula for angular momentum                               
                              (>! angle-chan acc')
                              ;;(om/update! (om/ref-cursor (:angle (om/root-cursor app-state))) [acc'])
                              (recur acc' (:bpm (:controls @app-state)) (util/current-time) (<! (timeout 10))))))))



