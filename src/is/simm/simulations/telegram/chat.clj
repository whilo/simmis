(ns is.simm.simulations.telegram.chat
  (:require [is.simm.runtimes.telegram :refer [send-text!]]
            [datahike.api :as d]
            [scicloj.clay.v2.api :as clay]))






(def schema [{:db/ident :text
              :db/valueType :db.type/string
              :db/cardinality :db.cardinality/one
              :db/doc "The text of the message"}])

(def conn (d/connect {:store {:backend :memory :id "12382392"}}))

(def members (atom {"Christian" {:id 1 :name "Christian"}
                    "simmis" {:id 2 :name "simmis"}}))

(defn receive-message [msg]
  (let [text (get msg :text)]
  ;; dispatch on message, create new user simulation if needed
    (when (not (contains? @members (get msg :from)))
      (swap! members assoc (get msg :from) (new-user (get msg :from))))

    (d/transact conn [{:text text}])
    (send-text! text)))

(defn start [])

(defn pause [])

(defn stop [])