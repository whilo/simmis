(ns is.simm.meta
  (:require [pattern :refer [compile-pattern rule]]))


(defmacro defn+
  ;; copied from defn in clojure.core
  "Same as `defn`, but also retains args (code) in metadata under :args."
  [name & args]
  `(do
     (defn ~name ~@args)
     (alter-meta! (var ~name) assoc :args (quote ~args))
     (var ~name)))

(defn update-time [ctx  name time]
  (swap! ctx #(-> %
                  (update-in [:accounts name :runtime] (fnil + 0) time)
                  (update-in [:accounts name :invocations] (fnil + 0) 1))))

(defmacro defsim 
  "Simulation macro, measures time spent in code."
  [name [ctx & args] & body]
  `(defn+ ~name [~ctx ~@args]
     (let [start-nanos# (System/nanoTime)]
       (let [result# (do ~@body)]
         (update-time ~ctx '(var ~name) (- (System/nanoTime) start-nanos#))
         result#))))



(comment

  (get-in (meta (var foo)) [:args])

  (defsim foo [ctx a b]
    (+ a b))

  (def my-ctx (atom {}))

  (dotimes [i 10000000]
    (foo my-ctx 1 2))

  161695520

  )

(defmacro defrel
  "Define a relation between inputs and outputs"
  [name variables matchers]

  )


(comment

  (defrel add [a b c]
    ;; standard function definition
    ([int? int? symbol?] (+ a b))
    ;; symbolic representation of the relation
    ([symbol? symbol? symbol?] '(= (+ a b) c))
    ;; infer input given output
    ([symbol? int? int?] '(= a (- c b)))
    ;; infer input given output
    ([int? symbol? int?] '(= b (- c a)))
    
    )

  
    

  (macroexpand-1
   '(defn+ foo [a b]
      (+ a b)))

  (defn+ foo [a b]
    (+ a b))

  (defn foos [a b] (+ a b))

  (meta #'foos)

  (meta #'foo)

  (meta #'defn)



  )