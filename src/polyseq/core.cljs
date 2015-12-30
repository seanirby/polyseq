(ns polyseq.core
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:require [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [polyseq.sound :as sound]
            [polyseq.util :as util]
            [cljs.core.async :refer [timeout mult tap untap put! chan <! >!]]
            [clojure.string :as string]))

(enable-console-print!)

(def preview-sound (sound/build #(this-as this (.pause this))))
(def angle-chan (chan))
(def control-chan (chan))
(def angle-mult (mult angle-chan))
(def control-mult (mult control-chan))
(def num-circles 15)

(defonce app-state 
  (atom {:circles (util/make-circles num-circles)
         :sequence-controls {:bpm 60
                             :on false}
         :sound-controls {:mode :writing
                          :sound-params (:params preview-sound)}}))

(defn handle-wave-change [e data]
  (let [k (keyword (js->clj (.. e -target -value)))]
    (om/update! data :wave k)))

(defn handle-point-click
  [{sound-params :sound-params is-active :is-active :as data-beat}
   {mode :mode sound-control-params :sound-params :as data-sound-control}
   sound]
  (if (= mode :reading)
    (when is-active
      (om/update! data-sound-control :sound-params @sound-params))
    (do (om/update! data-beat (merge @data-beat {:is-active true :sound-params @sound-control-params}))
        (sound/update! sound @sound-control-params))))

(defn mode-button [mode data]
  (let [base-class "mode-btn"
        opposite (if (= mode :reading) :writing :reading)
        is-active (= mode (:mode data))
        classes (if is-active (str base-class " active") base-class)]
    (dom/button #js {:className classes
                     :disabled (not is-active)
                     :onClick #(om/update! data :mode opposite)}
      (clojure.string/upper-case (name mode)))))

(defn wave-option [wave]
  (dom/option #js {:value (name wave)} (wave sound/waves)))

(defn key-view [{:keys [color frequency]} data]
  (let [is-active (= frequency (:frequency data))]
    (dom/div #js {:className (util/get-key-classes color is-active)
                  :onClick #(om/update! data (merge @data {:frequency frequency}))
                  :onMouseOver #((sound/update! preview-sound (merge @data {:frequency frequency}))
                                 (sound/play! preview-sound))})))

(defn octave-view [octave data]
  (dom/div #js {:className "octave"} (mapv #(key-view % data) octave)))

(defn sound-controls-view [{:keys [sound-params] :as data} owner]
  (reify
    om/IRender
    (render [_]
      (dom/div #js {:className "controls"}
        [(dom/div nil (str "Frequency: " (:frequency sound-params)))
         (dom/div #js {:className "piano"}
           (mapv #(octave-view % sound-params) (partition 12 util/piano)))
         (dom/div nil
           (dom/label nil "Wave: "
             (dom/select #js {:value (name (:wave sound-params))
                              :onChange #(handle-wave-change % sound-params)}
               (mapv wave-option (keys sound/waves)))))
         (dom/label nil "Release: "
           (dom/input #js {:type "range"
                           :min "5"
                           :max "2000"
                           :value (:release sound-params)
                           :onChange #(om/update! sound-params :release (int (js->clj (.. % -target -value))))}))
         (dom/div nil "Mode: "
           (mode-button :reading data)
           (mode-button :writing data))
         (dom/div nil
           (dom/button nil "Preview"))]))))

(defn sequence-controls-view [data owner]
  (let [control-chan (:control-chan (om/get-shared owner))]
    (reify
      om/IRender
      (render [_]
        (dom/div #js {:className "controls"}
          (dom/div nil
            (dom/label nil "Speed: "
              (dom/input #js {:type "range"
                              :max 400
                              :min 20
                              :value (:bpm data)
                              :onChange #(om/update! data :bpm (.. % -target -value))})))
          (dom/div nil
            (dom/button #js {:onClick (fn []
                                        (put! control-chan
                                          (fn [data] (assoc data :is-active false))))}
              "Clear All"))
          (dom/div nil
            (dom/button #js {:onClick (fn []
                                        (put! control-chan
                                          (fn [data]
                                            (let [is-active (rand-nth [true false])
                                                  sound-params (when is-active (sound/random-params (mapv :frequency util/piano) 1000))]
                                              (merge data {:is-active is-active :sound-params sound-params})))))}
              "Random Sequence"))
          (dom/div nil
            (dom/button #js {:onClick #(om/transact! data :on not)} "Start/Stop")))))))

(defn point-view [{:keys [data circle-frequency circle-radius]} owner]
  (let [{:keys [is-active is-playing angle beat sound-params]} data
        on-end  #(this-as this
                   (.pause this)
                   (om/update! data :is-playing false))
        sound (sound/build (:sound-params @data) on-end)]
    (reify
      om/IInitState
      (init-state [_]
        (let [chans (om/get-shared owner)]
          {:control-chan (tap (:control-mult chans) (chan))
           :angle-chan (tap (:angle-mult chans) (chan))}))
      om/IWillMount
      (will-mount [_]
        (let [angle-chan (:angle-chan (om/get-state owner))
              control-chan (:control-chan (om/get-state owner))]
          ;;Listen for angle changes, could support reverse speeds with a bit more logic
          (go-loop [angle (<! angle-chan)]
            (when (and
                    (not (:is-playing @data))
                    (not (nil? sound))
                    (:is-active @data)
                    (<= (- angle 20) (util/beat-to-angle beat circle-frequency) angle))
              (when-not (= (:sound-params @data) (:params sound))
                (sound/update! sound (:sound-params @data)))
              (sound/bang-play! sound)
              (om/update! data :is-playing true))
            (recur (<! angle-chan)))
          ;;Listen for control message, TODO, add an identifier to single out updates
          (go-loop [f (<! control-chan)]
            (om/transact! data f)
            (sound/update! sound (:sound-params @data))
            (recur (<! control-chan)))))
      om/IWillUnmount
      (will-unmount [_]
        (let [state (om/get-state owner)
              chans (om/get-shared owner)]
          (untap (:angle-mult chans) (:angle-chan state))
          (untap (:control-mult chans) (:control-chan state))))
      om/IRender
      (render [_]
        (let [sound-controls (om/ref-cursor (:sound-controls (om/root-cursor app-state)))
              angle (util/beat-to-angle beat circle-frequency)]
          (dom/div #js {:style (util/point-style angle circle-radius is-active)
                        :className (if is-playing "point playing" "point")
                        :onClick #(handle-point-click data (om/ref-cursor (:sound-controls (om/root-cursor app-state))) sound)}))))))

(defn circle-view [{:keys [frequency radius offset beats] :as data} owner]
  (reify
    om/IRender
    (render [_]
      (let [style (util/circle-style frequency radius offset)]
        (dom/div #js {:style style :className "circle"}
          (om/build-all point-view (mapv #(hash-map :data % :circle-frequency frequency :circle-radius radius) beats)))))))

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

(defn circles-view [{:keys [circles sound-controls sequence-controls] :as data} owner]
  (reify
    om/IRender
    (render [_]
      (apply dom/div nil [(om/build-all circle-view circles)
                          (om/build hand-view nil)
                          (dom/div #js {:className "controls-container"}
                            (om/build sound-controls-view sound-controls)
                            (om/build sequence-controls-view sequence-controls))]))))

(om/root circles-view
  app-state
  {:target (.getElementById js/document "app")
   :shared {:angle-mult angle-mult
            :control-chan control-chan
            :control-mult control-mult}})

;; Cycles through values 0-360 at a rate based on the apps 'bpm' value
;; Puts current angle value onto the angle channel so points can listen
;; and when they need to play their sound.
(defonce angle-clock ((fn []
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





