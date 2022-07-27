(ns racertay.light
  (:require [racertay.tuple :as tup]
            [racertay.color :as color]))

(defn point-light [position intensity]
  {:position position
   :intensity intensity})

(defn position [light]
  (:position light))

(defn intensity [light]
  (:intensity light))

(defn eq? [light1 light2]
  (and
   (tup/tup-eq? (:position light1) (:position light2))
   (color/color-eq? (:intensity light1) (:intensity light2))))

