(ns polyseq.sound)

;; TODO Add timbre.js as dependency to this project.
(def T js/T)
(def waves {:sin "sin" :saw "saw" :triangle "tri" :pulse "pulse"})

(defn random-params [freqs release-max]
  {:wave (rand-nth (keys waves))
   :frequency (rand-nth freqs)
   :release (+ 50 (rand-int 1000))})

(defn build
  ([f]
   (build {:wave :sin :frequency 440 :release 500} f))
  ([{:keys [wave frequency release] :as params} f]
   (let [osc (T (wave waves) #js {:mul 0.1 :f frequency})
         env (T "env"
               #js {:table (clj->js [1 [0, release]])
                    :releaseNode 0}
               osc)]
     (.on env "ended" f)
     (.bang env)
     ;;Since timbre.js creates a sound by wrappping an oscillator with an envelope,
     ;;all sound operations(.play .pause etc) need to be performed on the envelope.
     ;;We'll add another duplicate reference to envelope to distinguish between
     ;;operations performed on the envelope and operations performed on the sound.
     {:osc osc :env env :sound env :params params})))

(defn update! [{:keys [osc env sound] :as this} {:keys [wave frequency release] :as params}]
  (println this)
  (println params)
  (set! (.-wave osc) (wave waves))
  (set! (.-freq osc) frequency)
  (set! (.-table env) (clj->js [1 [0 release]]))
  (.bang sound)
  (assoc this :params params))

(defn bang-play! [{:keys [sound]}]
  (.bang sound)
  (.play sound))

(defn play! [{:keys [sound]}]
  (.play sound))
