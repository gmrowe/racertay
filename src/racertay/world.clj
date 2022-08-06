(ns racertay.world
  (:require [racertay.sphere :as sphere]
            [racertay.protocols :as p]
            [racertay.color :as color]
            [racertay.material :as material]
            [racertay.intersection :as intersection]
            [racertay.tuple :as tup]
            [racertay.light :as light]
            [racertay.ray :as ray]))

(def empty-world
  #:world{:light nil
          :objects []})

(defn intersect-world [world ray]
  (sort-by :intersection/t
           (mapcat #(p/intersect % ray) (:world/objects world))))


(defn shadowed? [world point]
  (let [{:world/keys [light objects]} world
        point-to-light-vec (tup/tup-sub (:light/position light) point)
        distance (tup/magnitude point-to-light-vec)
        ray (ray/ray point (tup/normalize point-to-light-vec))]
    (boolean
     (when-let [hit (intersection/hit (intersect-world world ray))]
       (< (:intersection/t hit) distance)))))

(defn shade-hit [world comps]
  (let [{:intersection/keys [object point eyev normalv over-point]} comps]
    (material/lighting
     (:material object)
     (:world/light world)
     point
     eyev
     normalv
     (shadowed? world over-point))))

(defn color-at [world ray]
  (if-let [hit (intersection/hit (intersect-world world ray))]
    (shade-hit world (intersection/prepare-computations hit ray))
    color/black))

