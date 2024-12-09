(ns is.simm.languages.web-search
  (:require [is.simm.languages.dispatch :refer [create-downstream-msg-handler get-runtime]]
            [is.simm.meta :refer [defn+]]
            [clojure.spec.alpha :as s]))

(defprotocol WebSearch
  (-web-search [this terms]))

(extend-protocol WebSearch
  nil
  (-web-search [this terms]
    "" #_(throw (Exception. "Not implemented."))))

(def url? string?)

(s/fdef web-search
  :args (s/cat :ctx map? :terms string?)
  :ret url?)
(defn+ web-search
  "Conduct a web search and return an URL."
  [ctx terms]
  (-web-search (get-runtime ctx :web-search) terms))


(comment

  (time
   (dotimes [i 10000000]
     (web-search {} "vancouver weather")))

  (time
   (dotimes [i 10000000]
     (-web-search nil "vancouver weather")))

  )

(let [handler (create-downstream-msg-handler ::search)]
  (defn search [terms]
    (handler terms)))

(def trap identity)

(defn nop [])

(comment
  (require '[cloroutine.core :refer [cr]])

(let [c (cr {trap nop}  (do (trap (web-search nil "vancouver weather")) 42))]
  [(c) (c)])

  ((cr {} (if nil :then :else)))

;; TODO factor into test
  (require '[is.simm.runtimes.brave :refer [brave]])

  (let [in (chan)
        out (chan)]
    (brave [S nil [in out]])
    (binding [*chans* [in out]]
      (<?? S (search "vancouver weather"))))

  )