(ns racertay.world
  (:require [racertay.sphere :as sphere]))

(def empty-world
  {:light nil
   :objects []})

(defn intersect-world [world ray]
  (sort-by :t (mapcat #(sphere/intersect % ray) (:objects world))))
