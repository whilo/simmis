(ns is.simm.runtimes.openai
  "OpenAI effects.

   Languages: gen-ai
   This runtime is a substrate, i.e. it does not emit lower level messages and does not interfere with outgoing messages."
  (:require [libpython-clj2.require :refer [require-python]]
            [libpython-clj2.python :refer [py. py.. py.-] :as py]
            [com.stuartsierra.component :as component]
            [is.simm.runtimes.branching :as branching]
            [is.simm.languages.gen-ai :as gen-ai]
            [taoensso.timbre :refer [debug warn]]
            [is.simm.config :refer [config]]
            [clojure.core.async :refer [chan promise-chan pub sub put! <!] :as async]
            [superv.async :refer [S go-try go-loop-try <? put?]]
            [clojure.data.json :as json]
            [babashka.http-client :as http]
            [clojure.java.io :as io]
            [clojure.spec.alpha :as s])
  (:import [java.util Base64]
           [java.util.function Function]))

(def api-key (:openai-key config))

(defn encode-file [file-path]
  (with-open [input-stream (io/input-stream file-path)]
    (let [file-bytes (.readAllBytes input-stream)]
      (.encodeToString (Base64/getEncoder) file-bytes))))

(def headers
  {"Authorization" (str "Bearer " api-key)})

;; spec for openai messages
(s/def ::openai-message (s/keys :req-un [::role ::content]))
(s/def ::role string?)
(s/def ::content (s/or :text string? :image_url (s/keys :req-un [::url])))
(s/def ::url string?)

(s/fdef payload
  :args (s/cat :model string? :messages (s/coll-of ::openai-message :kind vector?))
  :ret string?)
(defn payload [model messages]
  (json/write-str
    {"model" model
     "messages" messages 
     #_[{"role" "user"
         "content" 
         [{"type" "text"
           "text" text}
          {"type" "image_url"
           "image_url" {"url" (str "data:image/jpeg;base64," base64-image)}}]}]
     ;"max_tokens" 300
     }))

(def window-sizes {"gpt-3.5-turbo-0125" 16384
                   "gpt-4-turbo" 128000
                   "gpt-4o" 128000
                   "gpt-4o-2024-08-06" 128000
                   "gpt-4o-mini" 128000 
                   "o1-preview" 128000
                   "o1-mini" 128000 })

(s/fdef chat
  :args (s/cat :model string? :messages (s/coll-of ::openai-message :kind vector?))
  :ret (s/cat :response string?))
(defn chat [model messages]
  (let [res (promise-chan)
        cf (http/post "https://api.openai.com/v1/chat/completions"
                      {:headers (assoc headers "Content-Type"  "application/json")
                       :body (payload model messages)
                       :async true})]
    (-> cf
        (.thenApply (reify Function
                      (apply [_ response]
                        (put! res (-> response
                                      :body
                                      json/read-str
                                      (get "choices")
                                      first
                                      (get "message")
                                      (get "content"))))))
        (.exceptionally (reify Function
                          (apply [_ e]
                            (put! res (ex-info "Error in OpenAI chat." {:type :error-in-openai :error e}))))))
    res))


(s/fdef text-chat 
  :args (s/cat :model string? :text string?)
  :ret (s/cat :response string?))
(defn text-chat [model text]
  (let [res (chan)]
    (if (>= (count text) (* 4 (window-sizes model)))
      (do (warn "Text too long for " model ": " (count text) (window-sizes model))
          (put! res (ex-info "Sorry, the text is too long for this model. Please try a shorter text." 
                             {:type ::text-too-long :model model :text-start (subs text 0 100) :count (count text)}))
          res)
      (chat model [{"role" "user" 
                    "content" [{"type" "text" "text" text}]}]))))

(comment

  (async/<!! (text-chat "gpt-4o-mini" "What is the capital of France?"))

  (println
   (async/<!! (text-chat "gpt-4o-mini" "You are bootstrapping an AGI system on top of a distributed simulation engine with a git-like memory model in Clojure with Datahike. You can hook into APIs and export a user interface. ")))

  (def business-ideas
    (async/<!! (text-chat "gpt-4o-mini" "You are bootstrapping profitable economic systems on top of an AGI engine. You can hook into APIs and export a user interface. What business ideas could you persue? Pick ideas that can be executed with low effort, low risk and do not require a large investment. Maximize profits.")))

  (println business-ideas)

  (def goals
    (async/<!! (text-chat "gpt-4o-mini" "What goals should AGI pursue beyond typical human goals?")))

  (println goals)

;; vision chat
  (import '[java.nio.file Files Paths]
          '[java.util Base64])

  (def test-image "/home/ubuntu/screenshots/frames_0003.jpg")

  (defn read-file-as-byte-array [file-path]
    (Files/readAllBytes (Paths/get file-path (make-array String 0))))

  (defn encode-to-base64 [byte-array]
    (let [encoder (Base64/getEncoder)]
      (.encodeToString encoder byte-array)))

  (def test-base64 (encode-to-base64 (read-file-as-byte-array test-image)))

  (def test-image-query (create :model "gpt-4o-mini" :messages [{:role "user" :content [{:type "text" :text "What is in this image? Describe all the visual information as precisely as possible."} {:type "image_url" :image_url {:url (str "data:image/jpeg;base64," test-base64)}}]}]))


  )

;; TODO port these also to babashka
(require-python '[openai :refer [OpenAI]])

(def client (OpenAI :api_key (:openai-key config)))

(def create (py.- (py.- (py.- client chat) completions) create))


(defn py-chat [model text]
  (if (>= (count text) (* 4 (window-sizes model)))
    (do
      (warn "Text too long for " model ": " (count text) (window-sizes model))
      (throw (ex-info "Sorry, the text is too long for this model. Please try a shorter text." {:type ::text-too-long :model model :text-start (subs text 0 100) :count (count text)})))
    (let [res (create :model model :messages [{:role "system" :content text}])]
      (py.- (py.- (first (py.- res choices)) message) content))))


(defn image-gen [model text]
  (let [res ((py.- (py.- client images) generate) :model model :prompt text)]
    (py.- (first (py.- res data)) url)))

(comment 
  (image-gen "dall-e-3" "a dog playing in a small house")
  
  )

(comment
  (defn stt [model input-path]
    (let [audio-file ((py.- (py.- (py.- client audio) transcriptions) create) :model model :file ((py/path->py-obj "builtins.open") input-path "rb"))]
      (py.- audio-file text)))
  )

(defn stt [model input-path]
  (let [res (promise-chan)
        request (http/post "https://api.openai.com/v1/audio/transcriptions"
                           {:headers headers
                            :multipart [{:name "file" :content (io/file input-path) :file-name input-path :mimetype "audio/wav"}
                                        {:name "model" :content model}
                                        {:name "language" :content "en"}]
                            :async true})]
    (-> request
        (.thenApply (reify Function
                      (apply [_ response]
                        (put! res
                              (-> response
                                  :body
                                  json/read-str
                                  (get "text"))))))
        (.exceptionally (reify Function
                          (apply [_ e]
                            (put! res (ex-info "Error in OpenAI STT." {:type :error-in-openai :error e}))))))
    res))

(comment
  (require '[missionary.core :as m])

  (m/? (<! (stt-2 "whisper-1" "/tmp/microphone.wav")))

  )


(defn whisper-1 [input-path]
  (stt "whisper-1" input-path))




(comment
  (require-python '[pathlib :refer [Path]])

  (defn tts-1 [text]
    (let [res ((py.- (py.- (py.- client audio) speech) create) :model "tts-1" :voice "alloy" :input text)
          rand-path (str "/tmp/" (java.util.UUID/randomUUID) ".mp3")]
      ((py.- res stream_to_file) (Path rand-path))
      rand-path))

  )

;; same but with http client
(defn tts-1 [text]
  (let [res (promise-chan)
        request (http/post "https://api.openai.com/v1/audio/speech"
                           {:headers (assoc headers "Content-Type" "application/json")
                            :body (json/write-str {:model "tts-1" :voice "alloy" :input text})
                            :async true
                            :as :stream})]
    (-> request
        (.thenApply (reify Function
                      (apply [_ response]
                        (let [rand-path (str "/tmp/" (java.util.UUID/randomUUID) ".mp3")]
                          (io/copy (:body response) (io/file rand-path))
                          (put! res rand-path)))))
        (.exceptionally (reify Function
                          (apply [_ e]
                            (put! res (ex-info "Error in OpenAI TTS." {:type :error-in-openai :error e}))))))
    res))


(defn openai [[S peer [in out]]]
  (let [p (pub in (fn [{:keys [type]}]
                    (or ({:is.simm.languages.gen-ai/cheap-llm ::gpt-4o-mini
                          :is.simm.languages.gen-ai/reasoner-llm ::gpt-4o
                          :is.simm.languages.gen-ai/stt-basic ::whisper-1
                          :is.simm.languages.gen-ai/image-gen ::dall-e-3} type)
                        :unrelated)))
        gpt-4o-mini (chan)
        _ (sub p ::gpt-4o-mini gpt-4o-mini)

        gpt-4o (chan)
        _ (sub p ::gpt-4o gpt-4o)

        whisper-1 (chan)
        _ (sub p ::whisper-1 whisper-1)

        dall-e-3 (chan)
        _ (sub p ::dall-e-3 dall-e-3)

        next-in (chan)
        _ (sub p :unrelated next-in)]
    ;; TODO use async http requests for parallelism
    ;; TODO factor dedicated translator to LLM language
    (go-loop-try S [{[m] :args :as s} (<? S gpt-4o-mini)]
                 (when s
                   (go-try S
                           (put? S out (assoc s
                                              :type :is.simm.languages.gen-ai/cheap-llm-reply
                                              :response 
                                              (<! (text-chat "gpt-4o-mini" m)) 
                                              #_(try (py-chat "gpt-4o-mini" m) (catch Exception e e)))))
                   (recur (<? S gpt-4o-mini))))

    (go-loop-try S [{[m] :args :as s} (<? S gpt-4o)]
                 (when s
                   (go-try S
                           (put? S out (assoc s
                                              :type :is.simm.languages.gen-ai/reasoner-llm-reply
                                              :response 
                                              (<! (text-chat "gpt-4o-2024-08-06" m)) 
                                              #_(try (py-chat "gpt-4o-2024-08-06" m) (catch Exception e e)))))
                   (recur (<? S gpt-4o))))

    (go-loop-try S [{[m] :args :as s} (<? S whisper-1)]
                 (when s
                   (go-try S
                           (put? S out (assoc s
                                              :type :is.simm.languages.gen-ai/stt-basic-reply
                                              :response (try (stt "whisper-1" m) (catch Exception e e)))))
                   (recur (<? S whisper-1))))

    (go-loop-try S [{[m] :args :as s} (<? S dall-e-3)]
                 (when s
                   (go-try S
                           (put? S out (assoc s
                                              :type :is.simm.languages.gen-ai/image-gen-reply
                                              :response (try (image-gen "dall-e-3" m) (catch Exception e e)))))
                   (recur (<? S dall-e-3))))

    [S peer [next-in out]]))

(defrecord OpenAIRuntime [state]
  component/Lifecycle
  (start [this] this)
  (stop [this] this)
  branching/Branching
  (-branch [this id] (OpenAIRuntime. (atom (merge @(:state this) {:id id}))))
  branching/Merging
  (-merge [this other id] 
    (when-not (= @(:state this) @(:state other))
      (throw (ex-info "Merging incompatible runtimes." {:type :incompatible-runtimes
                                                        :this @(:state this)
                                                        :other @(:state other)})))
    (OpenAIRuntime. (atom (merge @(:state this) {:id id})))))

(defn openai-runtime [config]
  (OpenAIRuntime. (atom (merge {:usage {:last-request-time 0
                                        :num-requests 0}
                                :credits {:usd-per-month 120}}
                               config))))

(extend-protocol gen-ai/GenAI
  OpenAIRuntime
  (-cheap-llm [this msg]
    (let [{:keys [openai-key]} @(:state this)]
      (swap! (:state this) #(-> %
                                (assoc-in [:usage :last-request-time] (System/currentTimeMillis))
                                (update-in [:usage :num-requests] inc)))
      (text-chat "gpt-4o-mini" msg)))

  (-reasoner-llm [this msg]
    (let [{:keys [openai-key]} @(:state this)]
      (swap! (:state this) #(-> %
                                (assoc-in [:usage :last-request-time] (System/currentTimeMillis))
                                (update-in [:usage :num-requests] inc)))
      (text-chat "gpt-4o-2024-08-06" msg)))

  (-stt-basic [this voice-path]
    (let [{:keys [openai-key]} @(:state this)]
      (swap! (:state this) #(-> %
                                (assoc-in [:usage :last-request-time] (System/currentTimeMillis))
                                (update-in [:usage :num-requests] inc)))
      (try (stt "whisper-1" voice-path) (catch Exception e e))))

  (-image-gen [this prompt]
    (let [{:keys [openai-key]} @(:state this)]
      (swap! (:state this) #(-> %
                                (assoc-in [:usage :last-request-time] (System/currentTimeMillis))
                                (update-in [:usage :num-requests] inc)))
      (try (image-gen "dall-e-3" prompt) (catch Exception e e)))))