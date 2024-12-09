(ns is.simm.languages.gen-ai
   "Providing high-level generative AI functions for the runtime.
   
   The syntax is async (go-routine) functions that invoke
   lower level runtimes/downstreams with a piece of derived syntax (IR) and handle the replies transparently."
   (:require [is.simm.languages.dispatch :refer [create-downstream-msg-handler get-runtime]]
             [clojure.spec.alpha :as s]))


(let [handler (create-downstream-msg-handler ::cheap-llm)]
  (defn cheap-llm [msg]
    (handler msg)))

(let [handler (create-downstream-msg-handler ::reasoner-llm)]
  (defn reasoner-llm [msg]
    (handler msg)))

(let [handler (create-downstream-msg-handler ::stt-basic)]
  (defn stt-basic [voice-path]
    (handler voice-path)))

(let [handler (create-downstream-msg-handler ::image-gen)]
  (defn image-gen [prompt]
    (handler prompt)))


(defprotocol GenAI
  (-cheap-llm [this msg]
    "Generates text from a prompt using a cheap language model.")
  (-reasoner-llm [this msg]
    "Generates text from a prompt using a reasoner language model.")
  (-stt-basic [this voice-path]
    "Converts a voice file to text.")
  (-image-gen [this prompt]
    "Generates an image from a prompt."))

(def msg? string?)
(def voice-path? string?)
(def prompt? string?)

(s/fdef cheap-llm
  :args (s/cat :ctx map? :msg msg?)
  :ret msg?)
(defn cheap-llm-
  "Generates text from a prompt using a cheap language model."
  [ctx msg]
  (-cheap-llm (get-runtime ctx :gen-ai) msg))

(s/fdef reasoner-llm
  :args (s/cat :ctx map? :msg msg?)
  :ret msg?)
(defn reasoner-llm-
  "Generates text from a prompt using a reasoner language model."
  [ctx msg]
  (-reasoner-llm (get-runtime ctx :gen-ai) msg))

(s/fdef stt-basic
  :args (s/cat :ctx map? :voice-path voice-path?)
  :ret msg?)
(defn stt-basic-
  "Converts a voice file to text."
  [ctx voice-path]
  (-stt-basic (get-runtime ctx :gen-ai) voice-path))

(s/fdef image-gen
  :args (s/cat :ctx map? :prompt prompt?)
  :ret msg?)
(defn image-gen-
  "Generates an image from a prompt."
  [ctx prompt]
  (-image-gen (get-runtime ctx :gen-ai) prompt))


(comment

  (require '[is.simm.runtimes.openai :refer [openai]])

;; TODO needs to use pub-sub
  (let [in (chan)
        out (chan)]
    (openai [S nil [in out]])
    (binding [*chans* [in out]]
      (<?? S (cheap-llm "What is the capital of Ireland?"))))
  )