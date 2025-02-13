(ns is.simm.prompts
  (:require [clojure.java.io :as io]))

(def assistance (slurp (io/resource "prompts/assistance.txt")))

(def summarization (slurp (io/resource "prompts/summarization.txt")))

(def note (slurp (io/resource "prompts/note.txt")))

(def search (slurp (io/resource "prompts/search.txt")))

(def imitate (slurp (io/resource "prompts/imitate.txt")))

(def screen (slurp (io/resource "prompts/screen.txt")))

(def minecraft (slurp (io/resource "prompts/minecraft.txt")))
