(ns is.simm.runtimes.ubuntu
  "Control an Ubuntu 22.04 desktop machine."
  (:require  [is.simm.runtimes.openai :refer [text-chat chat whisper-1 tts-1]]
             [is.simm.prompts :as prompts]
             [clojure.core.async :refer [timeout put! chan pub sub close! take! poll! go-loop go] :as async]
             [taoensso.timbre :refer [debug info warn] :as log]
             [missionary.core :as m]
             [libpython-clj2.require :refer [require-python]]
             [libpython-clj2.python :refer [py. py.. py.-] :as py]
             [hyperfiddle.rcf :refer [tests]]

             [clojure.edn :as edn]
             [clojure.spec.alpha :as s]
             [clojure.string :as str]
             [clojure.java.io :as io]
             [clojure.java.shell :as shell])
  (:import [java.util Base64]))


;; ===== Helpers =====

;; throw on shell error
(defn shell [cmd & args]
  (let [result (apply shell/sh cmd args)]
   (if (zero? (:exit result))
     (:out result)
     (throw (ex-info "Shell command failed" result)))))

(defn open [filename]
  (shell "xdg-open" filename))

(defn encode-file [file-path]
  (with-open [input-stream (io/input-stream file-path)]
    (let [file-bytes (.readAllBytes input-stream)]
      (.encodeToString (Base64/getEncoder) file-bytes))))

(defn >! "Puts given value on given channel, returns a task completing with true when put is accepted, of false if port was closed."
  [c x] (doto (m/dfv) (->> (async/put! c x))))

(defn <! "Takes from given channel, returns a task completing with value when take is accepted, or nil if port was closed."
  [c] (doto (m/dfv) (->> (async/take! c))))

#_(tests
   (m/? (<! (whisper-1 "/tmp/microphone.wav"))) := "Hello, 1, 2, 3, 4, 5, 6, 7.")

(defn play-audio [filename]
  (m/via m/blk (shell "cvlc" "--no-loop" "--play-and-exit" filename)))

(comment
  (shell "cvlc" "--no-loop" "/tmp/58a6708e-6fa3-41eb-b75b-e3cc4934e064.mp3"))
  

;; ===== Language =====

(defn vlm 
  ([prompt filename]
   (<! (chat "gpt-4o"
             [{:type "text"
               :text prompt}
              {:type "image_url"
               :image_url {:url (str "data:image/jpeg;base64," (encode-file filename))}}])))
  ([prompt-template filename audio-in audio-out screen-in action-out]
   (let [prompt (format prompt-template
                       (str/join "\n" (take-last 100 audio-in))
                       (str/join "\n== Screen ==\n" (take-last 10 screen-in))
                       (str/join "\n" (take-last 100 audio-out))
                       (str/join "\n" (take-last 100 action-out)))]
     #_(println prompt)
     (<! (chat "gpt-4o" [{:type "text" :text prompt}
                         {:type "image_url"
                          :image_url {:url (str "data:image/jpeg;base64," (encode-file filename))}}])))))

(defn llm [prompt-template audio-in audio-out screen-in action-out]
  (let [prompt (format prompt-template
                       (str/join "\n" (take-last 100 audio-in))
                       (str/join "\n== Screen ==\n" (take-last 10 screen-in))
                       (str/join "\n" (take-last 100 audio-out))
                       (str/join "\n" (take-last 100 action-out)))]
    #_(println prompt)
    (<! (chat "gpt-4o" [{:type "text" :text prompt}]))))


(comment

  (m/? (llm prompts/screen
            ["This is a nice day." "Yesterday it rained."]
            ["Hello, I am a computer." "I am here to help you."]
            ["A weather forecast for tomorrow."]
            [])))
  
  
;; ===== Perception =====

(defn screenshot [filename]
  (m/via m/blk (shell "gnome-screenshot" "--window" "-f" filename)))

(def screenshot-prompt "You are helping a Minecraft agent by describing everything visible on the screen. Describe all the visual information as precisely as possible such that it can be retained even if the image is lost. Describe the screen hierarchically. If a menu is open describe everything necessary to navigage it and always include the inventory if visible. Write down *all* text you can see in the respective context. Summarize your observations in the end and try to infer what the user is doing on a high-level.")

;; listen to raw ubuntu microphone
(defn listen-to-microphone []
  (shell "arecord" "-f" "cd" "-t" "wav" "-d" "10" "-q" "-r" "16000" "-c" "1" "-D" "pulse" "/tmp/microphone.wav"))

(defn listen-to-speakers [interval]
  (m/sp
   (let [device "alsa_input.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__hw_sofhdadsp_6__source"
         #_"alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__hw_sofhdadsp__sink.monitor"
         #_"bluez_sink.F8_DF_15_4F_1D_F0.a2dp_sink.monitor"
        ;; random file name 
         filename (str "/tmp/speakers-" (rand-int 1000000) ".mp3")]
     (m/? (m/via m/blk (shell "ffmpeg" "-f" "pulse" "-i" device "-t" (str interval) filename)))
     filename)))


(comment
  (listen-to-microphone)

  (listen-to-speakers 10)

  (def whisper-test (whisper-1 "/tmp/microphone.wav"))
  
  (def tts-test (tts-1 "Hello, I am a computer."))

  (shell "vlc" tts-test))


;; Audio
(defn audio-iterator
  [interval]
  (m/ap
   (loop []
     (let [recording (m/? (listen-to-speakers interval))]
       (m/amb recording (recur))))))

(defn audio-listen [!audio-in interval]
  (let [<x (audio-iterator interval)]
    (m/reduce (fn [_ f]
                (when f
                  (debug "audio: " f)
                  (let [text (m/? (<! (whisper-1 f)))]
                    (debug "audio text: " text)
                    (swap! !audio-in conj [f text]))))
              nil <x)))


;; Video
(defn screen-iterator 
  []
  (m/ap
    (loop []
     (let [filename (str "/tmp/screenshot-" (rand-int 1000000) ".png")
           _  (m/? (screenshot filename))]
       (m/amb filename (recur))))))

(defn screen-watch [!screen-in interval]
  (let [<x (screen-iterator)]
    (m/reduce (fn [_ f]
                (when f
                  (debug "screenshot: " f)
                  ;; iterate with at least interval seconds
                  (m/? (m/join vector
                               (m/sleep (* interval 1000))
                               (m/sp
                                (let [text (time (m/? (vlm screenshot-prompt f)))]
                                  #_(debug "screen text: " text)
                                  (swap! !screen-in conj [f text])))))))
                  
              nil <x)))

(comment


  (def !audio-in (atom []))

  (def listen-flow (audio-listen !audio-in 10))

  (def dispose-listen!
    (listen-flow
     #(prn ::success %)
     #(prn ::error %)))

  (dispose-listen!)

  (def !screen-in (atom []))

  (def watch-flow (screen-watch !screen-in 30))

  (def dispose-watch! (watch-flow prn prn))

  (dispose-watch!))

  

;; ===== Action ===== 

(require-python '[time :as time])
(require-python '[evdev :refer [UInput ecodes]])


(def clj->ecode
  {:space (py.- ecodes KEY_SPACE)
   :enter (py.- ecodes KEY_ENTER)
   :left (py.- ecodes KEY_LEFT)
   :right (py.- ecodes KEY_RIGHT)
   :up (py.- ecodes KEY_UP)
   :down (py.- ecodes KEY_DOWN)
   :shift (py.- ecodes KEY_LEFTSHIFT)
   :ctrl (py.- ecodes KEY_LEFTCTRL)
   :alt (py.- ecodes KEY_LEFTALT)

   :a (py.- ecodes KEY_A)
   :b (py.- ecodes KEY_B)
   :c (py.- ecodes KEY_C)
   :d (py.- ecodes KEY_D)
   :e (py.- ecodes KEY_E)
   :f (py.- ecodes KEY_F)
   :g (py.- ecodes KEY_G) 
   :h (py.- ecodes KEY_H) 
   :i (py.- ecodes KEY_I) 
   :j (py.- ecodes KEY_J) 
   :k (py.- ecodes KEY_K) 
   :l (py.- ecodes KEY_L) 
   :m (py.- ecodes KEY_M) 
   :n (py.- ecodes KEY_N) 
   :o (py.- ecodes KEY_O) 
   :p (py.- ecodes KEY_P) 
   :q (py.- ecodes KEY_Q) 
   :r (py.- ecodes KEY_R) 
   :s (py.- ecodes KEY_S) 
   :t (py.- ecodes KEY_T) 
   :u (py.- ecodes KEY_U) 
   :v (py.- ecodes KEY_V) 
   :w (py.- ecodes KEY_W) 
   :x (py.- ecodes KEY_X) 
   :y (py.- ecodes KEY_Y) 
   :z (py.- ecodes KEY_Z)

   :1 (py.- ecodes KEY_1)
   :2 (py.- ecodes KEY_2)
   :3 (py.- ecodes KEY_3)
   :4 (py.- ecodes KEY_4)
   :5 (py.- ecodes KEY_5)
   :6 (py.- ecodes KEY_6)
   :7 (py.- ecodes KEY_7)
   :8 (py.- ecodes KEY_8)
   :9 (py.- ecodes KEY_9)
   :0 (py.- ecodes KEY_0)})

(defn press-key [key duration]
  (py/with-gil-stack-rc-context
    (let [key (clj->ecode key)
          ui (UInput)]
      (if key
        (do
          (py. ui write (py.- ecodes EV_KEY) key 1)  ;; Key press
          (py. ui syn)
          (time/sleep duration)
          (py. ui write (py.- ecodes EV_KEY) key 0)  ;; Key release
          (py. ui syn)
          (py. ui close))
        (warn "Invalid key" key))
      (py. ui close))))

(defn mouse-move [position]
  (py/with-gil-stack-rc-context
    (let [[x y] position
          ui (UInput {(py.- ecodes EV_REL) [(py.- ecodes REL_X) (py.- ecodes REL_Y)]})]
      (if (and x y)
        (do
          (py. ui write (py.- ecodes EV_REL) (py.- ecodes REL_X) x)
          (py. ui write (py.- ecodes EV_REL) (py.- ecodes REL_Y) y))
        (warn "Invalid mouse position" position))
      (py. ui syn)
      (py. ui close))))

(defn mouse-click [button duration]
  (py/with-gil-stack-rc-context
    (let [key ({:left-click (py.- ecodes BTN_LEFT)
                :right-click (py.- ecodes BTN_RIGHT)} button)
          ui (UInput {(py.- ecodes EV_KEY) [(py.- ecodes BTN_LEFT) (py.- ecodes BTN_RIGHT)]})]
      (if key
        (do
          (py. ui write (py.- ecodes EV_KEY) key 1)
          (py. ui syn)
          (time/sleep duration)
          (py. ui write (py.- ecodes EV_KEY) key 0)
          (py. ui syn))
        (warn "Invalid mouse button" button))
      (py. ui close))))

(comment
  (press-key :a 0.1)

  (mouse-move [13 500])

  (mouse-click :right-click 2.0)
  
  )
  

;; ===== Agent =====

(defn silence? [s]
  (or (str/blank? s) (= (.toLowerCase s) "you")))

(defn baseline-0 []
  (let [!audio-in (atom [])
        !audio-out (atom [])
        !screen-in (atom [])]
    ;; run audio and screen perception in parallel
    (m/race (audio-listen !audio-in 10)
            (screen-watch !screen-in 30)

            (m/sp
             (loop []
               (debug "talk loop")
               (when (silence? (second (last @!audio-in)))
                 (let [statement (m/? (llm (str prompts/screen
                                                "If there is nothing new to say then reply with QUIET. Otherwise say the next sentence:\n")
                                           (map second @!audio-in)
                                           (map second @!audio-out)
                                           (map second @!screen-in)
                                           ;; no actions for this baseline
                                           []))]
                   (debug "statement" statement)
                   (when-not (.contains statement "QUIET")
                     (swap! !audio-out conj statement)
                     (m/? (play-audio (m/? (<! (tts-1 statement))))))))
               (m/? (m/sleep 10000))
               (recur))))))

;; Problems
;; - it is talking all the time, fixed by adding a silence check
;; - it captures its own audio output
;; - it seems to focus too much on the screen and not on recent audio input, fixed by prompt refiningYou support the user.


(comment

  (log/set-min-level! :debug)

  (def baseline-0-test (baseline-0))

  (def baseline-0-dispose
    (baseline-0-test
     #(prn ::success %)
     #(prn ::error %)))

  (baseline-0-dispose))
  

(s/def ::actions (s/coll-of (s/or :key (s/keys :req-un [::key ::duration])
                                  :mouse (s/keys :req-un [::mouse]))))

(defn parse-spec [input spec default]
 (try
   (let [p (second (.split input "```clojure"))
         p (first (.split p "```"))
         p (edn/read-string p)]
     (if (s/valid? spec p) p default))
   (catch Exception _ [])))

(defn baseline-1 []
  (let [!audio-in (atom [])
        !audio-out (atom [])
        !screen-in (atom [])
        !action-out (atom [])]
    ;; run audio and screen perception in
    (m/race (audio-listen !audio-in 10)
            (screen-watch !screen-in 30)

            (m/sp
             (loop []
               (debug "talk loop")
               (when true #_(silence? (second (last @!audio-in)))
                 (let [statement (m/? (llm (str prompts/minecraft
                                                "If there is nothing new to say then reply with QUIET. Otherwise say the next two sentences from the first person perspective in a fun and playful style:\n")
                                           (map second @!audio-in)
                                           @!audio-out
                                           (map second @!screen-in)
                                           @!action-out))]
                   #_(debug "statement" statement)
                   (when-not (.contains statement "QUIET")
                     (swap! !audio-out conj statement)
                     (m/? (play-audio (m/? (<! (tts-1 statement))))))))
               (m/? (m/sleep 10000))
               (recur)))

            (m/sp
             (loop []
               (debug "action loop")
               (m/? (m/sleep 60000))
               (let [last-screen (first (last @!screen-in))
                     _ (debug "last-screen" last-screen)
                     raw-actions (m/? (vlm (str prompts/minecraft
                                                "Given this context and latest screenshot, provide a long and robust action sequence for the next 60 seconds in Clojure edn only, e.g.: [{:key :a :duration 0.3}, {:key :1 :duration 0.1}, {:key :space :duration 0.1}, {:mouse :left-click :duration 5.0}, {:mouse :move :position [13 500]}]. You can provide an empty vector if there is nothing to do.:\n")
                                           last-screen
                                           (map second @!audio-in)
                                           @!action-out
                                           (map second @!screen-in)
                                           @!action-out))
                     actions (parse-spec raw-actions ::actions [])]
                 (debug "actions" raw-actions actions)
                 (doseq [{:keys [key duration mouse position] 
                          :as action} actions]
                   (swap! !action-out conj action)
                   (cond key (press-key key duration)
                         (and mouse position) (mouse-move position)
                         (and mouse duration) (mouse-click mouse duration)))
                (m/? (m/sleep 5000))
                (recur)))))))


;; Observations & problems
;; - commentary is ok, but has long pauses and is 3rd person; change prompt to 1st person
;; - action loop is very slow
;; - mouse clicks still don't work

(comment

  (def baseline-1-test (baseline-1))

  (def baseline-1-dispose
    (baseline-1-test
     #(prn ::success %)
     #(prn ::error %)))

  (baseline-1-dispose)
  
  )
  