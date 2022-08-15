(ns racertay.world
  (:require [racertay.protocols :as p]
            [racertay.color :as color]
            [racertay.material :as material]
            [racertay.intersection :as intersection]
            [racertay.tuple :as tup]
            [racertay.light :as light]
            [racertay.fcmp :as fcmp]
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

(declare color-at)

(defn- calc-reflected-color [world reflectv over-point reflective remaining]
  (let [reflect-ray (ray/ray over-point reflectv)
        color (color-at world reflect-ray (dec remaining))]
    (color/color-mul-scalar color reflective)))

(def max-mutual-recursion-depth 5)

(defn reflected-color
  ([world comps]
   (reflected-color world comps max-mutual-recursion-depth))
  ([world comps remaining]
   (let [{:intersection/keys [object over-point reflectv]} comps
         reflective (get-in object [:material :material/reflective])]
     (if (or (< remaining 1) (fcmp/nearly-zero? reflective))
       color/black
       (calc-reflected-color world reflectv over-point reflective remaining)))))

(defn shade-hit
  ([world comps]
   (shade-hit world comps max-mutual-recursion-depth))
  ([world comps remaining]
   (let [{:intersection/keys [object point eyev normalv over-point]} comps
         shadowed (shadowed? world over-point)
         surface  (material/lighting
                   (:material object) object (:world/light world) over-point
                   eyev normalv shadowed)
         reflected (reflected-color world comps remaining)]
     (color/color-add surface reflected))))

(defn color-at
  ([world ray]
   (color-at world ray max-mutual-recursion-depth))
  ([world ray remaining]
   (if-let [hit (intersection/hit (intersect-world world ray))]
     (shade-hit world (intersection/prepare-computations hit ray) remaining)
     color/black)))
