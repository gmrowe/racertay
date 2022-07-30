(ns racertay.world
  (:require [racertay.sphere :as sphere]
            [racertay.protocols :as p]))

(def empty-world
  {:light nil
   :objects []})

(defn intersect-world [world ray]
  (sort-by :intersection/t (mapcat #(p/intersect % ray) (:objects world))))
