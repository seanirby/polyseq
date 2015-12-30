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
(def angle-mult (mult angle-chan))
(def control-chan (chan))
(def control-mult (mult control-chan))
(def num-circles 15)
(defonce app-state 
  (atom {:sequence-controls {:bpm 60
                             :on false}
         :circles (mapv util/make-circle-data (range 1 (+ 1 num-circles) 1))
         :sound-controls {:mode :writing
                          :sound-params (:params preview-sound)}}))

(defn get-key-classes [color is-active]
  (let [base-class "key "
        color-class (if (= color :white) "key-white " "key-black ")
        active-class (if is-active "key-active" "")]
    (str base-class color-class active-class)))

(defn key-view [{:keys [color frequency]} sound-params]
  (let [classes (get-key-classes color (= frequency (:frequency sound-params)))]
    (dom/div #js {:className classes
                  :onClick #(om/update! sound-params (merge sound-params {:frequency frequency}))
                  :onMouseOver #(do (sound/update! preview-sound (merge sound-params {:frequency frequency}))
                                    (sound/play! preview-sound))})))

(defn octave-view [octave sound-params]
  (dom/div #js {:className "octave"} (mapv #(key-view % sound-params) octave)))

(defn build-mode-button [mode data]
  (let [base-class "mode-btn"
        opposite (if (= mode :reading) :writing :reading)
        is-active (= mode (:mode data))
        classes (if is-active (str base-class " active") base-class)]
    (dom/button #js {:className classes
                     :disabled (not is-active)
                     :onClick #(om/update! data :mode opposite)} (clojure.string/upper-case (name mode)))))

(defn build-wave-option [wave]
  (dom/option #js {:value (name wave)} (wave sound/waves)))

(defn handle-wave-change [e data]
  (let [k (keyword (js->clj (.. e -target -value)))]
    (om/update! data :wave k)))

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
                              :onChange #(do (println (.. % -target -value)) (handle-wave-change % sound-params))}
               (mapv build-wave-option (keys sound/waves)))))
         (dom/label nil "Release: "
           (dom/input #js {:type "range"
                           :min "5"
                           :max "2000"
                           :value (:release sound-params)
                           :onChange #(om/update! sound-params :release (int (js->clj (.. % -target -value))))}))
         (dom/div nil "Mode: "
           (build-mode-button :reading data)
           (build-mode-button :writing data))
         (dom/div nil
           (dom/button nil "Preview"))]))))

(defn handle-point-click [{:keys [sound-params is-active] :as beat-data} {mode :mode control-sound-params :sound-params :as sound-control-data}]
  (if (= mode :reading)
    (when is-active
      (om/update! sound-control-data :sound-params sound-params))
    (om/update! beat-data (merge beat-data {:is-active true :sound-params control-sound-params}))))

(defn point-view [{:keys [data circle-frequency circle-radius]} owner]
  (let [{:keys [is-active is-playing angle beat sound-params]} data]
    (reify
      om/IInitState
      (init-state [_]
        {:control-chan (tap (:control-mult (om/get-shared owner)) (chan))
         :angle-chan (tap (:angle-mult (om/get-shared owner)) (chan))})
      om/IWillMount
      (will-mount [_]
        (let [on-end  #(this-as this
                         (.pause this)
                         (om/update! data :is-playing false))
              angle-chan (:angle-chan (om/get-state owner))
              control-chan (:control-chan (om/get-state owner))]
          ;;Listen for angle changes, could support reverse speeds with a bit more logic
          (go-loop [sound (if (nil? (:sound-params @data))
                            (sound/build on-end)
                            (sound/build (:sound-params @data) on-end))
                    angle (<! angle-chan)]
            (let [sound' (when-not (nil? (:sound-params @data))
                           (cond (and (nil? sound) (not (nil? (:sound-params @data)))) (sound/build (:sound-params @data) on-end)
                                 (= (:params sound) (:sound-params @data)) sound
                                 :else (sound/update! sound (:sound-params @data))))]
              (when (and
                      (not (:is-playing @data))
                      (not (nil? sound'))
                      (:is-active @data)
                      (<= (- angle 20) (* 360 (/ beat circle-frequency)) angle))
                (sound/bang-play! sound')
                (om/update! data :is-playing true))
              (recur sound' (<! angle-chan))))
          ;;Listen for control message, TODO, add an identifier to single out updates
          (go-loop [f (<! control-chan)]
            (om/transact! data f)
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
                        :onClick #(do (handle-point-click data
                                        (om/ref-cursor (:sound-controls (om/root-cursor app-state)))))
                        :onMouseUp #()
                        :onMouseDown #()}))))))


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
             (dom/button #js {:onClick #(om/transact! data :on not)} "Start/Stop"))])))))

(defn circles-view [{:keys [circles sound-controls sequence-controls] :as data} owner]
  (reify
    om/IRender
    (render [_]
      (apply dom/div nil [(om/build-all circle-view circles)
                          [(om/build hand-view nil)
                           (dom/div #js {:className "controls-container"}
                             (om/build sound-controls-view sound-controls)
                             (om/build sequence-controls-view sequence-controls))]]))))

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





