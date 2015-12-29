(ns polyseq.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [polyseq.util :as util]
            [cljs.core.async :refer [timeout mult tap untap put! chan <! >!]]
            [clojure.string :as string]))

(enable-console-print!)

(let [on-end #(this-as this (.pause this))]
  (def preview-release 500)
  (def preview-frequency 440)
  (def preview-osc (js/T "saw" #js {:mul 0.25 :f preview-frequency}))
  (def preview-env (.on (js/T "env"
                          #js {:table [1 [0, preview-release]]
                               :releaseNode 0}
                          preview-osc)
                     "ended"
                     on-end)))
(def angle-chan (chan))
(def angle-mult (mult angle-chan))
(def control-chan (chan))
(def control-mult (mult control-chan))
(def num-circles 15)
(defonce app-state 
  (atom {:sequence-controls {:bpm 60
                    :on false}
         :circles (mapv util/make-circle-data (range 1 (+ 1 num-circles) 1))
         :sound-controls {:display true
                   :mode :writing
                   :release preview-release
                   :frequency preview-frequency}}))

(defn get-key-classes [color is-active]
  (let [base-class "key "
        color-class (if (= color :white) "key-white " "key-black ")
        active-class (if is-active "key-active" "")]
    (str base-class color-class active-class)))

(defn preview-sound [frequency release]
  (set! (.-freq preview-osc) frequency)
  (set! (.-table preview-env) (clj->js [1 [0 release]]))
  (.bang preview-env)
  (.play preview-env))

(defn get-sound-wave [frequency]
  (js/T "pulse" #js {:freq frequency :mul 0.1}))

(defn get-sound-enveloped [wave release]
  (js/T "perc" #js {:r release} wave))

(defn get-sound [frequency release]
  (let [on-end #(this-as this (.pause this))]
    (.on (get-sound-enveloped (get-sound-wave frequency) release)  "ended" on-end)))

(defn key-view [{:keys [color frequency]} sound-controls]
  (let [f (:frequency sound-controls)
        release (:release sound-controls)
        classes (get-key-classes color (= frequency f))]
    (dom/div #js {:className classes
                  :onMouseOver #(preview-sound frequency release)})))

(defn octave-view [octave sound-controls]
  (dom/div #js {:className "octave"} (mapv #(key-view % sound-controls) octave)))

(defn build-mode-button [mode data]
  (let [base-class "mode-btn"
        opposite (if (= mode :reading) :writing :reading)
        is-active (= mode (:mode data))
        classes (if is-active (str base-class " active") base-class)]
    (dom/button #js {:className classes
                     :disabled (not is-active)
                     :onClick #(om/update! data :mode opposite)} (clojure.string/upper-case (name mode)))))

(defn sound-controls-view [data owner]
  (reify
    om/IRender
    (render [_]
      (when (:display data)
        (dom/div #js {:className "controls"}
          [(dom/div nil (str "Frequency: " (:frequency data)))
           (dom/div #js {:className "piano"}
             (mapv #(octave-view % data) (partition 12 util/piano)))
           (dom/label nil "Release: "
             (dom/input #js {:type "range"
                             :min "5"
                             :max "2000"
                             :value (:release data)
                             :onChange #(om/update! data :release (int (js->clj (.. % -target -value))))}))
           (dom/div nil "Mode: "
             (build-mode-button :reading data)
             (build-mode-button :writing data))
           (dom/div nil
             (dom/button nil "Preview"))])))))

(defn point-view [{:keys [beat frequency is-active is-playing circle-frequency circle-radius] :as data} owner]
  (reify
    om/IInitState
    (init-state [_]
      {:control-chan (tap (:control-mult (om/get-shared owner)) (chan))
       :angle-chan (tap (:angle-mult (om/get-shared owner)) (chan))})
    om/IWillMount
    (will-mount [_]
      (let [on-end #(this-as this (.pause this) (om/update! data :is-playing false))
            sound (.on (get-sound-enveloped (get-sound-wave frequency) (get-in @app-state [:sound-controls :release]))  "ended" on-end)
            angle-chan (:angle-chan (om/get-state owner))
            control-chan (:control-chan (om/get-state owner))]
        ;;Listen for angle changes, could support reverse speeds with a bit more logic
        (go-loop [angle (<! angle-chan)]
          (when (and
                  (not (:is-playing @data))
                  (:is-active @data)
                  (<= (- angle 20) (* 360 (/ beat circle-frequency)) angle))
            (.play (.bang sound))
            (om/update! data :is-playing true))
          (recur (<! angle-chan)))
        ;;Listen for control message, TODO, add an identifier to single out updates
        (go-loop [{:keys [message]} (<! control-chan)]
          (om/update! data (merge data message))
          (println "message received")
          (recur (<! control-chan)))))
    om/IWillUnmount
    (will-unmount [_]
      (untap (:angle-mult (om/get-shared owner)) (:angle-chan (om/get-state owner)))
      (untap (:control-mult (om/get-shared owner)) (:control-chan (om/get-state owner))))
    om/IRender
    (render [_]
      (let [sound-controls (om/ref-cursor (:sound-controls (om/root-cursor app-state)))
            clazz (if is-playing "point playing" "point")
            style (util/point-style (* beat (/ 360 circle-frequency)) circle-radius is-active)]
        (dom/div #js {:className clazz
                      :style style
                      ;;:onClick #(om/update! data :is-active (not is-active))
                      :onMouseUp #()
                      :onMouseDown #()})))))


(defn circle-view [{:keys [frequency radius offset beats] :as data} owner]
  (reify
    om/IRender
    (render [_]
      (let [style (util/circle-style frequency radius offset)]
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

(defn sequence-controls-view [data owner]
  (let [control-chan (:control-chan (om/get-shared owner))]
    (reify
      om/IRender
      (render [_]
        (dom/div #js {:className "controls"} nil
          [(dom/div nil
             (dom/label nil "Speed: "
               (dom/input #js {:type "range"
                               :max 400
                               :min 20
                               :value (:bpm data)
                               :onChange #(om/update! data :bpm (.. % -target -value))})))
           (dom/div nil
             (dom/button #js {:onClick #(put! control-chan {:message {:is-active false}})} "Clear All"))
           (dom/div nil
             (dom/button #js {:onClick #(om/transact! data :on not)} "Start/Stop"))])))))

(defn circles-view [{:keys [circles sound-controls sequence-controls] :as data} owner]
  (reify
    om/IRender
    (render [_]
      (apply dom/div nil [(om/build-all circle-view circles)
                          [(om/build hand-view nil)
                           (om/build sound-controls-view sound-controls)
                           (om/build sequence-controls-view sequence-controls)]]))))

(om/root circles-view
  app-state
  {:target (.getElementById js/document "app")
   :shared {:angle-mult angle-mult
            :control-chan control-chan
            :control-mult control-mult}})

(defonce angle-trigger ((fn []
                          (go-loop [acc 0
                                    bpm (:bpm (:sequence-controls @app-state))
                                    t (util/current-time)
                                    _ (<! (timeout 10))]
                            (if (and (get-in @app-state [:sequence-controls :on]) (.hasFocus js/document))
                              (let [elapsed (/ (- (util/current-time) t) 1000)
                                    acc' (mod (+ acc (* bpm 0.02)) 360)] ;;TODO get the actual formula for angular momentum                               
                                (>! angle-chan acc')
                                (recur acc' (:bpm (:sequence-controls @app-state)) (util/current-time) (<! (timeout 10))))
                              (recur acc (:bpm (:sequence-controls @app-state)) (util/current-time) (<! (timeout 10))))))))





