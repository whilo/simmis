(ns is.simm.runtime
  (:require [sci.core :as sci]
            [clojure.walk :as walk]
            [cloroutine.impl :as cloroutine]
            [is.simm.runtimes.openai :as openai]
            [babashka.http-client :as babashka-http]))

(defn lift-ns [sym]
  (let [ens (sci/create-ns sym)
        publics (ns-publics sym) ]
    (update-vals publics #(sci/copy-var* % ens))))

(defn sci-var? [x]
  (instance? sci.lang.Var x))

(defn atom? [x]
  (instance? clojure.lang.Atom x))

(defn init [id]
  (let [ctx (sci/init {:namespaces {'cloroutine.impl (lift-ns 'cloroutine.impl)
                                    'is.simm.runtimes.openai (lift-ns 'is.simm.runtimes.openai)}})]
    (sci/eval-form ctx '(defmacro cr [breaks & body]
                          (cloroutine.impl/compile (gensym "cr") breaks &env (cons `do body))))
    (assoc ctx :id id)))

;; TODO this is still shallow, and it should also probably deal with refs and datahike connections
(defn copy-mutables [v]
  (walk/postwalk (fn [v]
                   (if (atom? v) (atom @v) v))
                 v))

(defn copy [ctx id]
  (let [copied (sci/fork ctx)]
    (swap! (:env copied) update :namespaces (fn [nss]
                                              (->>
                                               (for [[ns publics] nss]
                                                 [ns (->>
                                                      (for [[s v] publics]
                                                        (if (sci-var? v)
                                                          ;; TODO copy metadata
                                                          [s (sci/new-var s (copy-mutables @v))]
                                                          [s v]))
                                                      (into {}))])
                                               (into {}))))
    (assoc copied :id id)))


(comment
   ;; test fork isolation
  (let [ctx1 (init :ctx1)
        _ (sci/eval-form ctx1 '(def state (atom 0)))
        ctx2 (copy ctx1 :ctx2)]
    (sci/eval-form ctx1 '(reset! state 42))
    (sci/eval-form ctx2 '(reset! state 43))
    [(sci/eval-form ctx1 '(deref state)) (sci/eval-form ctx2 '(deref state))]
    #_[(get-in @(:env ctx1) [:namespaces 'user 'state])
       (get-in @(:env ctx2) [:namespaces 'user 'state])])

  (def ctx (init :openai))

  (sci/eval-form ctx '(require '[is.simm.runtimes.openai :as openai]))

  (time
   (dotimes [i 10000]
     (copy ctx :openai2)))

  (time (do (copy ctx :openai2) nil))

  (require '[clojure.core.async :as async])

  (async/<!! (sci/eval-form ctx '(openai/text-chat "gpt-4o-mini" "Hello, how are you?")))


  )
  






(comment
(sci/eval-string "(inc 1)") 

(def ctx (sci/init {}))

(sci/copy-ns ctx 'clojure.tools.analyzer.jvm)

(sci/eval-form ctx '(def foos 42))


(sci/eval-form ctx '(let [a 42] (/ 42 0)))


(time
 (sci/eval-form ctx '(last (map inc (range 100000000)))))


(sci/fork ctx)

  (sci/eval-form ctx '(defmacro defn+
  ;; copied from defn in clojure.core
  "Same as `defn`, but also retains args (code) in metadata under :args."
  [name & args]
  `(do
     (defn ~name ~@args)
     (alter-meta! (var ~name) assoc :args (quote ~args))
     (var ~name))))

(sci/eval-form ctx '(defn+ foos [a b] (+ a b)))



(let [ens (sci/create-ns 'cloroutine.impl)
      publics (ns-publics 'cloroutine.impl)
      sci-ns (update-vals publics #(sci/copy-var* % ens))
      ctx (sci/init {:namespaces {'cloroutine.impl sci-ns}})]
  (sci/eval-string* ctx "
#_(ns cloroutine.core (:require [cloroutine.impl :as i]))
(defmacro cr [breaks & body]
  (cloroutine.impl/compile (gensym) breaks &env (cons `do body)))")
  
  
  (def ctx ctx)
  )

(sci/eval-form ctx '(defn foo [a b] (+ a b)))

(sci/eval-form ctx '(foo 1 2))

(sci/eval-form ctx '(defn nop []))


(sci/eval-form ctx '(i/compile (gensym "cr") '{user/foo user/nop} {} '(do (user/foo (+ 1 2)) 42)))

(def c (sci/eval-form ctx '(cr {user/foo user/nop} (do (user/foo 1 2) 42))))

(let [cs (atom nil)]
  (c (fn [x] (reset! cs x)))
  [(@cs) (@cs)])


(sci/eval-form ctx '(require '[cloroutine.impl :as i]))

(sci/eval-form ctx '(import '[java.lang Exception]))

(def bar (sci/eval-form ctx (sci/eval-form ctx '(i/compile (gensym "cr") {} {} '(do (+ 1 2))))))

(bar)


(sci/fork)

(defn atom? [x]
  (instance? clojure.lang.Atom x))

(atom? (atom 42))

(def foo-clj (sci/eval-form ctx '(defn foo [a b] (+ a b)) #_'(cloroutine.impl.analyze-clj/analyze {} '(defn foo [a b] (+ a b)))))

(foo-clj 1 2)

(sci/eval-string* ctx (slurp "/home/christian/Development/cloroutine/src/cloroutine/impl.cljc"))




(sci/eval-form ctx '(require '[clojure.core.async :as async]))
)

;; how to lift effectors?
;; 1. either we map vars into a namespace directly [do this for now, it is more directly mapping the interpreter interface]
;; 2. or we use dedicted effect runtimes that are passed to the simulation

(require '[missionary.core :as m])

((m/seed [1 2 3]) prn prn)



;; how to deal with streaming relations inside the simulation?
;; 1. use in and out flows of missionary
;; streams that run in the simulation must be manageable from the outset
;; 2. use core.async channels [use this for now as core.async is supported on both sides]
;; provide an in out channel for each external information source 
;; processing needs to be confirmed from the interpreter side
;; 3. just invoke a function from the runtime (maybe with callback)
;; this is the simplest way, probably the easiest to synthesize code and easy to fork a runtime because runtime is stateless once all calls have returned
;; allows to write code in pure functional style

;; problem snapshot isolation vs. strong consistency of snapshots
;; requiring pause of running system to synchronize, does not scale and will create prohibitive pauses
;; the problem with snapshot isolation will be that the system might have processed messages that were not visible for the observer

;; on which granularity is the simulation run?
;; 1. each external system/agent/process is represented by a simulation
;; 2. subsystems are represented by nested simulations
;; subsystems need to be able to replay, but external subsystems are only partially observable

;; example telegram bot
;; 1. telegram bot provides a partially observable interface to telegram and runs in a simulation
;; 2. each chat room is spawned as a nested simulation
;; 3. each user context is spawned as a nested simulation

;; simulations can be merged between different encapsulating systems, e.g. a telegram user can also be the operator of a screen
;; merge interpreter namespaces and runtimes
;; or provide messaging
;; spawn nested simulations eagerly or lazily?


;; how do nested simulations work?
;; e.g. we simulate execution of each user context in a separate simulation
;; but we also want to simulate groups of users

;; reify simulations as a value



(comment 

  
  (def telegram-chat
    (let [ns-sym 'is.simm.runtimes.telegram
          ens (sci/create-ns ns-sym)
          publics (ns-publics ns-sym)
          sci-ns (update-vals publics #(sci/copy-var* % ens))
          ctx (sci/init {:namespaces {ns-sym sci-ns}})]
      (sci/eval-string* ctx (slurp "src/is/simm/simulations/telegram_chat.clj"))))
  
)