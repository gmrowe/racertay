(ns racertay.intersection
  (:require [racertay.ray :as ray]
            [racertay.tuple :as tup]
            [racertay.fcmp :as fcmp]))

(defn intersection [t obj]
  #:intersection{:t t
                 :object obj})

(def intersections vector)
(def empty-intersections (intersections))

(defn hit [inters]
  (when-let [hits (seq (filter #(pos? (:intersection/t %)) inters))]
    (apply min-key :intersection/t hits)))
