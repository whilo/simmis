(ns is.simm.missionary-utils
  "Utilities for working with missionary."
  (:require [missionary.core :as m]
            [clojure.core.async :as async]
            
            [hyperfiddle.rcf :refer [tests]])
  (:import [java.util.function Function]))


(defn >! "Puts given value on given channel, returns a task completing with true when put is accepted, of false if port was closed."
  [c x] (doto (m/dfv) (->> (async/put! c x))))

(defn <! "Takes from given channel, returns a task completing with value when take is accepted, or nil if port was closed."
  [c] (doto (m/dfv) (->> (async/take! c))))

(defn throwable? [x]
  (instance? Throwable x))

(defn go->task "Takes from given channel, returns a task completing with value when take is accepted, or nil if port was closed. Rethrows throwables."
  [c] 
  (m/sp
   (let [res (m/? (<! c))]
     (if (throwable? res)
       (throw res)
       res))))

(defn cf->task [cf]
  (let [dfv (m/dfv)]
    (-> cf
        (.thenApply (reify Function
                      (apply [_ response]
                        (dfv response))))
        (.exceptionally (reify Function
                          (apply [_ e]
                            (dfv (ex-info "Fetch transcript data  error." {:type :error-in-fetch-transcript-data :error (ex-message e)}))))))
    (m/sp
     (let [res (m/? dfv)]
       (if (throwable? res)
         (throw res)
         res)))))

(defn fastest [& args]
  (m/absolve (apply m/race (map m/attempt args))))

(comment

  (let [c (async/chan)]
    (async/put! c (ex-info "test error" {:type :test-error}))
    (m/? (go->task c)))

  )

(tests
;; roundtrip test error
 (let [c (async/chan)
       e (ex-info "test error" {:type :test-error})]
   (async/put! c e)
   (:type (ex-data (try (m/? (go->task c)) (catch Exception e e))))) := :test-error
 
  ;; roundtrip test value
  (let [c (async/chan)
        v :test-value]
    (async/put! c v)
    (m/? (go->task c))) := :test-value
 )
 