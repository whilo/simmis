(ns is.simm.simulations.telegram.user
  "This namespace simulates a user in a Telegram chat room."
  {:language :sci}
  (:require
   [is.simm.simulations.telegram.chat :refer [send-text!]]
   [clojure.core.async :refer [go go-loop <! close! put!]]))


(defn dispatch [msg [in out]]
  (go
    (when-let [text (get msg :text)]
      (send-text! [in out] text))))

;; Simulator environment interface
(def state (atom nil))

(defn construct []
  (reset! state {}))

(defn run [[in out]]
  (swap! state assoc :chans [in out])
  (go-loop []
    (let [msg (<! in)]
      (when msg
        (<! (dispatch msg [in out]))
        (recur)))))

(defn pause []
  (go
    (let [[in out] @state]
      (close! in)
      (close! out)
      (swap! state dissoc :chans))))

(defn destruct []
  (reset! state nil))