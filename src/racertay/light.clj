(ns racertay.light
  (:require [racertay.tuple :as tup]
            [racertay.color :as color]))

(defn point-light [position intensity]
  {:light/position position
   :light/intensity intensity})

(defn eq? [light1 light2]
  (and
   (tup/tup-eq? (:light/position light1) (:light/position light2))
   (color/color-eq? (:light/intensity light1) (:light/intensity light2))))

