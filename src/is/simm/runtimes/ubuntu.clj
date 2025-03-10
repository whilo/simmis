(ns is.simm.runtimes.ubuntu
  "Control an Ubuntu 22.04 desktop machine."
  (:require  [is.simm.runtimes.openai :refer [text-chat chat whisper-1 tts-1]]
             [is.simm.runtimes.brave :as brave]
             [is.simm.runtimes.text-extractor :as text-extractor]
             [is.simm.prompts :as prompts]
             [is.simm.missionary-utils :refer [go->task] :as mu]
             [is.simm.config :as config]
             [clojure.core.async :refer [timeout put! chan pub sub close! take! poll! go-loop go] :as async]
             [taoensso.timbre :refer [debug info warn error] :as log]
             [missionary.core :as m]
             [libpython-clj2.require :refer [require-python]]
             [libpython-clj2.python :refer [py. py.. py.-] :as py]
             [hyperfiddle.rcf :refer [tests]]
             [datahike.api :as d]
             [selmer.parser :refer [render]]

             [clojure.spec.alpha :as s]
             [clojure.string :as str]
             [clojure.java.io :as io]
             [clojure.java.shell :as shell]
             [clojure.data.json :as json])
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

#_(tests
   (m/? (<? (whisper-1 "/tmp/microphone.wav"))) := "Hello, 1, 2, 3, 4, 5, 6, 7.")

(defn play-audio [filename]
  (m/via m/blk (shell "cvlc" "--no-loop" "--play-and-exit" filename)))

(comment
  (shell "cvlc" "--no-loop" "/tmp/58a6708e-6fa3-41eb-b75b-e3cc4934e064.mp3"))


;; ===== Language =====

(defn vlm
  ([prompt filename]
   (go->task (chat "chatgpt-4o-latest" #_"gpt-4o"
             [{:role "user" :content
               [{:type "text"
                 :text prompt}
                {:type "image_url"
                 :image_url {:url (str "data:image/jpeg;base64," (encode-file filename))}}]}])))
  ([prompt-template filename audio-in audio-out screen-in action-out]
   (let [prompt (format prompt-template
                        (str/join "\n" (take-last 100 audio-in))
                        (str/join "\n\n\n==== Screen transcript ====\n\n\n" (take-last 10 screen-in))
                        (str/join "\n" (take-last 100 audio-out))
                        (str/join "\n" (take-last 100 action-out)))]
     (go->task (chat "chatgpt-4o-latest" #_"gpt-4o"
               [{:role "user" :content
                 [{:type "text" :text prompt}
                  {:type "image_url"
                   :image_url {:url (str "data:image/jpeg;base64," (encode-file filename))}}]}])))))

(defn llm [prompt-template audio-in audio-out screen-in action-out]
  (let [prompt (format prompt-template
                       (str/join "\n" (take-last 100 audio-in))
                       (str/join "\n\n\n==== Screen transcript ====\n\n\n" (take-last 10 screen-in))
                       (str/join "\n" (take-last 100 audio-out))
                       (str/join "\n" (take-last 100 action-out)))]
    (go->task (chat "chatgpt-4o-latest" #_"gpt-4o"
              [{:role "user" :content
                [{:type "text" :text prompt}]}]))))


(comment

  (m/? (llm prompts/screen
            ["This is a nice day." "Yesterday it rained."]
            ["Hello, I am a computer." "I am here to help you."]
            ["A weather forecast for tomorrow."]
            [])))


;; ===== Perception =====

(defn screenshot [filename]
  (m/via m/blk (shell "gnome-screenshot" "--window" "-f" filename)))

(def screenshot-prompt "You are helping a computer user by describing everything visible in the screenshot. Describe all the visual information as precisely as possible such that it can be retained even if the image is lost. Describe the screen hierarchically. If a menu is open describe everything necessary to navigate it and always include the inventory if visible. Write down *all* text you can see in the respective context. Summarize your observations in the end and try to infer what the user is doing on a high-level.")


(defn listen-to-device [device interval]
  (m/sp
   (let [;; random file name 
         filename (str "/tmp/speakers-" (java.util.UUID/randomUUID) ".mp3")]
     (m/? (m/via m/blk (shell "ffmpeg" "-f" "pulse" "-i" device "-t" (str interval) filename)))
     filename)))

(def audio-devices (:audio config/config))

(comment

  (m/? (listen-to-device (:microphone audio-devices) 10))

  (def whisper-test (whisper-1 "/tmp/microphone.wav"))

  (def tts-test (m/? (go->task (tts-1 "Hello, I am a computer."))))

  (shell "vlc" tts-test))


;; Audio
(defn audio-iterator
  [device interval]
  (m/ap
   (loop []
     (let [recording (m/? (listen-to-device device interval))]
       (m/amb recording (recur))))))

(s/fdef audio-listen
  :args (s/cat :device string?
               :interval pos-int?
               :cb (s/fspec :args (s/cat :text string?)
                            :ret any?))
  :ret nil?)
(defn audio-listen [device interval cb]
  (let [<x (audio-iterator device interval)]
    (m/reduce (fn [_ f]
                (when f
                  (debug "audio: " f)
                  (try
                    (let [text (m/? (go->task (whisper-1 f)))
                          text (if (or (.contains text "for watching")
                                       (.contains text "Thank you")
                                       (= text "you"))
                                 "" text)]
                      (debug "audio text: " text)
                      (cb text)
                      nil)
                    (catch Exception e
                      (error "Cannot transcribe audio:" e f)))))
              nil <x)))


;; Video
(defn screen-iterator
  []
  (m/ap
   (loop []
     (let [filename (str "/tmp/screenshot-" (java.util.UUID/randomUUID) ".png")
           _  (m/? (screenshot filename))]
       (m/amb filename (recur))))))

(s/fdef screen-watch
  :args (s/cat :interval pos-int?
               :cb (s/fspec :args (s/cat :text string?)
                            :ret any?))
  :ret nil?)
(defn screen-watch [interval cb]
  (let [<x (screen-iterator)]
    (m/reduce (fn [_ f]
                (when f
                  (debug "screenshot: " f)
                  ;; iterate with at least interval seconds
                  (m/? (m/join vector
                               (m/sleep (* interval 1000))
                               (m/sp
                                (let [text "" #_(time (m/? (vlm screenshot-prompt f)))]
                                  #_(debug "screen text: " text)
                                  (cb f text)))))
                  nil))
              nil <x)))

(comment

  (def listen-flow (audio-listen (:microphone audio-devices) 10 #(println "audio:" %)))

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
   :forward (py.- ecodes KEY_UP)
   :backward (py.- ecodes KEY_DOWN)
   :shift (py.- ecodes KEY_LEFTSHIFT)
   :ctrl (py.- ecodes KEY_LEFTCTRL)
   :alt (py.- ecodes KEY_LEFTALT)
   :esc (py.- ecodes KEY_ESC)
   :escape (py.- ecodes KEY_ESC)

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

(defn press-keys [keys duration]
  (py/with-gil-stack-rc-context
    (let [keys (map clj->ecode keys)
          duration (or duration 0.1)
          ui (UInput)]
      (doseq [key keys
              :when key]
        (py. ui write (py.- ecodes EV_KEY) key 1))  ;; Key press
      (py. ui syn)
      (time/sleep duration)
      (doseq [key keys
              :when key]
        (py. ui write (py.- ecodes EV_KEY) key 0))  ;; Key release
      (py. ui syn)
      (py. ui close))))

(defn mouse-move [relative]
  (py/with-gil-stack-rc-context
    (let [[x y] relative
          ui (UInput {(py.- ecodes EV_KEY) [(py.- ecodes BTN_LEFT) (py.- ecodes BTN_RIGHT)]
                      (py.- ecodes EV_REL) [(py.- ecodes REL_X) (py.- ecodes REL_Y)]})]
      (if (and x y)
        (do
          (py. ui write (py.- ecodes EV_REL) (py.- ecodes REL_X) x)
          (py. ui write (py.- ecodes EV_REL) (py.- ecodes REL_Y) y))
        (warn "Invalid mouse relative" relative))
      (py. ui syn)
      (py. ui close))))

(defn mouse-click [button duration]
  (py/with-gil-stack-rc-context
    (let [key ({:left (py.- ecodes BTN_LEFT)
                :right (py.- ecodes BTN_RIGHT)} button)
          duration (or duration 0.1)
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
  (press-keys [:a] 0.1)

  (mouse-move [0 80])

  (dotimes [_ 10]
    (Thread/sleep 2000)
    (mouse-move [0 200]))

  (mouse-click :right 2.0)

  (let [[x y] [200 200]
        ui (UInput {(py.- ecodes EV_REL) [(py.- ecodes REL_X) (py.- ecodes REL_Y)]})]
    (if (and x y)
      (do
        (py. ui write (py.- ecodes EV_REL) (py.- ecodes REL_X) x)
        (py. ui write (py.- ecodes EV_REL) (py.- ecodes REL_Y) y)))
    (py. ui syn)
    (py. ui close))


  )


;; ===== Agent =====

(defn silence? [s]
  (or (str/blank? s) (= (.toLowerCase s) "you")))


;; datahike schema for events that will be fed to gpt4o
(def schema [{:db/ident :event/type
              :db/valueType :db.type/keyword
              :db/cardinality :db.cardinality/one}
             {:db/ident :event/created
              :db/valueType :db.type/instant
              :db/cardinality :db.cardinality/one}
             {:db/ident :event/role
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one}

             {:db/ident :audio/in
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one}
             {:db/ident :audio/out
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one}
             {:db/ident :audio/device
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one}

             {:db/ident :screen/file
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one}
             {:db/ident :screen/transcript
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one}

             {:db/ident :youtube/transcript
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one}
             {:db/ident :youtube/summary
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one}
             {:db/ident :youtube/id
              :db/valueType :db.type/string
              :db/unique :db.unique/identity
              :db/cardinality :db.cardinality/one}
             {:db/ident :youtube/title
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one}
             {:db/ident :youtube/description
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one}

             {:db/ident :assistant/output
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one}])


(comment

  ;; test with memory database
  (def config {:store {:backend :mem}})

  (def cfg (d/create-database config))

  (def conn (d/connect cfg))

  (d/transact conn schema)

  (d/transact conn [{:event/created (java.util.Date.)
                     :event/role "user"
                     :audio/in "Hello, I am a computer."
                     :audio/device "alsa_input.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__hw_sofhdadsp_6__source"}
                    {:event/created (java.util.Date.)
                     :event/role "developer"
                     :audio/out "Hello, I am a computer."
                     :audio/device "alsa_output.pci-0000_00_1f.3-platform-skl_hda_dsp_generic.HiFi__hw_sofhdadsp__sink.monitor"}
                    {:event/created (java.util.Date.)
                     :event/role "user"
                     :screen/file "/tmp/test_screenshot_123.png"}])


  (->> @conn
       (d/q '[:find ?s ?c :where [?e :screen/file ?s] [?e :event/created ?c]])
       (sort-by second)
       ffirst)

  ;; pull full event history out of conn
  (->> @conn
       (d/q '[:find (pull ?e [:*]) :where [?e :event/created ?c]])
       (map first)
       (sort-by :event/created)
       events->openai-messages))

(defn events->openai-messages [events]
  (map (fn [event]
         (let [role (get event :event/role)
               audio-in (get event :audio/in)
               audio-out (get event :audio/out)
               assistant-output (get event :assistant/output)
               screen-file (get event :screen/file)
               screen-transcript (get event :screen/transcript)
               youtube-summary (get event :youtube/summary)
               action (get event :action)
               created (get event :event/created)]
           (case (get event :event/type)
             :is.simm.runtimes.ubuntu/audio-in
             {:role role
              :content [{:type "text" :text (str created " audio-in (microphone): " audio-in)}]}

             :is.simm.runtimes.ubuntu/audio-out
             {:role role
              :content [{:type "text" :text (str created " audio-out (speaker): " audio-out)}]}

             :is.simm.runtimes.ubuntu/assistant-output
             {:role role
              :content [{:type "text" :text (str created " assistant-output: " assistant-output)}]}

             :is.simm.runtimes.ubuntu/screen-transcript
             {:role role
              :content [{:type "text" :text (str created " screen-transcript: " screen-transcript)}]}

             :is.simm.runtimes.ubuntu/youtube-summary
             {:role role
              :content [{:type "text" :text (str created " youtube-summary: " youtube-summary)}]}

             (do (error "missing event" event)
                 {:role role :content
                  [{:type "text" :text "Missing event."}]})

             #_(and role screen-file)
             #_{:role role
                :content [{:type "image_url"
                           :image_url {:url (str "data:image/jpeg;base64," (encode-file screen-file))}}]})))
       events))


;; a function that translates
;; https://www.youtube.com/watch?v=KHxZ3mT9BSo -> KHxZ3mT9BSo
(defn youtube-id [url]
  (str/replace url "https://www.youtube.com/watch?v=" ""))

(tests
 (youtube-id "https://www.youtube.com/watch?v=KHxZ3mT9BSo") := "KHxZ3mT9BSo")

(defn retrieve-youtube-transcript [conn title]
  (let [url (->> (brave/search-brave title)
                 :web
                 :results
                 (filter #(.startsWith (:url %) "https://www.youtube.com/watch?v="))
                 first
                 :url)
        id (when url (youtube-id url))]
    (debug "Youtube id:" id)
    (m/sp
     (when id
       (when-not (d/entity @conn [:youtube/id id])
         (try
           (let [_ (debug "extracting youtube transcript" id)
                 {:keys [title description transcript]} (m/? (text-extractor/youtube-transcript id))
                 _ (debug "youtube transcript" (subs transcript 0 100))
                 summary (m/? (go->task (chat "gpt-4o-mini"
                                              (vec [{:role "developer" :content
                                                     [{:type "text" :text "Summarize the following YouTube transcript comprehensively and use Wikilinks, e.g. [[New York]] or [[New York City][New York]] to refer to entities, events and important topics:"}
                                                      {:type "text" :text (str title "\n" description "\n" transcript)}]}]))))
                 _ (debug "youtube summary" summary)
                 _ (d/transact! conn [{:youtube/transcript transcript
                                       :youtube/title title
                                       :youtube/description description
                                       :youtube/summary summary
                                       :youtube/id id
                                       :event/type ::youtube-transcript
                                       :event/created (java.util.Date.)
                                       :event/role "developer"}])])
           (catch Exception e
             (debug "Could not extract youtube transcript" id e))))))))

(comment

  (m/? (retrieve-youtube-transcript nil))

  (m/? (text-extractor/youtube-transcript "KDorKy-13ak"))


  (m/? (text-extractor/youtube-transcript "1EfpN08mh9I"))

  )


(s/def ::actions (s/coll-of (s/or :statement (s/keys :req-un [::action ::text]
                                                     :opt-un [::duration])
                                  :retrieve-youtube-transcript (s/keys :req-un [::action ::title])
                                  :inner-monologue (s/keys :req-un [::action ::text])
                                  :retrieve-notes (s/keys :req-un [::titles])
                                  :predict-next-actions (s/keys :req-un [::next-actions])
                                  :press-keys (s/keys :req-un [::action ::keys])
                                  :mouse-move (s/keys :req-un [::action ::relative])
                                  :mouse-click (s/keys :req-un [::action ::button]
                                                       :opt-un [::duration]))))

(s/def ::action string?)
(s/def ::text string?)
(s/def ::title string?)
(s/def ::keys (s/coll-of string?))
(s/def ::duration pos?)
(s/def ::button string?)
(s/def ::relative (s/tuple number? number?))
(s/def ::next-actions (s/coll-of any?))
(s/def ::titles (s/coll-of string?))

(defn parse-json [input default]
  (try
    (let [p (if (.contains input "```json")
              (first (.split (second (.split input "```json")) "```"))
              input)
          p (json/read-str p :key-fn keyword)]
      (if (s/valid? ::actions p)
        p
        (do (warn "Could not validate:" p (s/explain ::actions p)) 
            default)))
    (catch Exception _ default)))

(defn assist [config]
  (let [{:keys [system-prompt user]
         db-config :db
         {:keys [in-interval out-interval]
          {:keys [microphone speakers]} :devices} :audio} config
        conn (d/connect db-config)]
    (mu/fastest (audio-listen microphone
                              (or in-interval 5)
                              #(d/transact! conn [{:audio/in %
                                                   :audio/device microphone
                                                   :event/type ::audio-in
                                                   :event/created (java.util.Date.)
                                                   :event/role "user"}]))

                (audio-listen speakers
                              (or out-interval 5)
                              #(d/transact! conn [{:audio/out %
                                                   :event/type ::audio-out
                                                   :audio/device speakers
                                                   :event/created (java.util.Date.)
                                                   :event/role "developer"}]))

                #_(screen-watch 10
                                #(d/transact! conn [{:screen/file %1
                                                     :screen/transcript %2
                                                     :event/type ::screen-transcript
                                                     :event/created (java.util.Date.)
                                                     :event/role "user"}]))

                (m/sp
                 (loop []
                   (debug "talk loop")
                   (let [#_#_last-screen (->> @conn
                                              (d/q '[:find ?s ?c :where [?e :screen/file ?s] [?e :event/created ?c]])
                                              (sort-by second)
                                              reverse
                                              first ;; newest
                                              first)
                         last-screen (str "resources/public/electric_starter_app/tmp/screenshot-" (java.util.UUID/randomUUID) ".png")
                         _ (m/? (screenshot last-screen))
                         _ (d/transact! conn [{:screen/file last-screen
                                               :event/type ::screenshot
                                               :event/created (java.util.Date.)
                                               :event/role "user"}])
                         messages (->> @conn
                                       (d/q '[:find (pull ?e [:*]) :where [?e :event/created ?c]])
                                       (map first)
                                       (sort-by :event/created)
                                       (take 200)
                                       events->openai-messages)]
                     (debug "last-screen" last-screen)

                     (if (and (seq messages) last-screen)
                         (let [output (m/? (go->task (chat #_"gpt-4o-mini" "chatgpt-4o-latest"
                                                           (vec (concat [{:role "developer" :content
                                                                          [{:type "text" :text (render system-prompt {:user user})}]}]
                                                                        messages
                                                                        [{:role "user" :content
                                                                          [{:type "text" :text "Current screenshot:"}
                                                                           {:type "image_url"
                                                                            :image_url {:url (str "data:image/jpeg;base64," (encode-file last-screen))}}]}])))))
                               _ (debug "system output" output)
                               actions (parse-json output [])]
                           (debug "parsed" actions)
                           (d/transact! conn [{:assistant/output output
                                               :event/type ::assistant-output
                                               :event/created (java.util.Date.)
                                               :event/role "developer"}])
                           (doseq [{:keys [action keys duration button relative text title] :as args} actions]
                             (try
                               (case action
                                 "statement" (m/? (play-audio (m/? (go->task (tts-1 text)))))
                                 "press-keys" (press-keys (map keyword keys) duration)
                                 "mouse-move" (mouse-move relative)
                                 "mouse-click" (mouse-click (keyword button) duration)
                                 "retrieve-youtube-transcript" (m/? (retrieve-youtube-transcript conn title))
                                 (warn "Not supported yet" action args))
                               (catch Exception e
                                 (error "Executing action: " e args))))
                           (m/? (m/sleep 5000)))
                         (m/? (m/sleep 1000))))
                   (recur))))))

(def default-assist-config {:user "Christian"
                            :system-prompt (slurp (io/resource "prompts/screen.txt"))
                            :audio {:devices {:microphone (:microphone audio-devices)
                                              :speakers (:speakers audio-devices)}
                                    :in-interval 5
                                    :out-interval 5}
                            :db {:store {:backend :file :path "/tmp/assist-default"}}})

(comment

  (log/set-min-level! :debug)


  (def cfg (d/create-database (:db plaicraft-assist-config)))

  (d/delete-database (:db plaicraft-assist-config))

  (def conn (d/connect (:db plaicraft-assist-config)))

  (d/transact conn schema)

  (->> @conn
       (d/q '[:find (pull ?e [:*]) :where [?e :event/created ?c]])
       (map first)
       (sort-by :event/created)
       reverse
       events->openai-messages)


  (def assist-test (assist default-assist-config))

  (def assist-dispose
    (assist-test
     #(prn ::success %)
     #(prn ::error %)))

  (assist-dispose)

  )

(def plaicraft-assist-config {:user "Jesse"
                              :system-prompt (slurp (io/resource "prompts/plaicraft-short.txt"))
                              :audio {:devices {:microphone (:microphone audio-devices)
                                                :speakers (:speakers audio-devices)}
                                      :in-interval 5
                                      :out-interval 5}
                              :db {:store {:backend :file :path "/tmp/assist-plaicraft"}}})


(defn -main [& _args]
  (let [db-config (:db plaicraft-assist-config)]
    (try
      (-> (d/create-database db-config)
          d/connect
          (d/transact schema))
      (catch Exception e
        (debug "Database not created." e)))
    (let [assist-test (assist plaicraft-assist-config)]
      (let [assist-dispose
            (assist-test
             #(prn ::success %)
             #(prn ::error %))]
             ;; sleep forever
        (async/<!! (chan))
        (assist-dispose)))))

(comment

  (-main)

  )