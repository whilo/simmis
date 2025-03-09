(ns is.simm.runtimes.branching
  (:refer-clojure :exclude [merge]))

(defprotocol Branching
  (-branch [this id] "Branches off current system with id."))

(defn branch [this id]
  (-branch this id))

(defprotocol Merging
  (-merge [this other id] "Merges  system with another with id."))

(defn merge [this other id]
  (-merge this other id))