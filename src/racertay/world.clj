(ns racertay.world
  (:require [racertay.protocols :as p]
            [racertay.color :as color]
            [racertay.material :as material]
            [racertay.computations :as comps]
            [racertay.intersection :as intersection]
            [racertay.tuple :as tup]
            [racertay.light :as light]
            [racertay.fcmp :as fcmp]
            [racertay.ray :as ray]
            [racertay.shape :as shape]))

(def empty-world
  #:world{:light nil
          :objects []})

(defn intersect-world [world ray]
  (sort-by :intersection/t
           (mapcat #(shape/intersect % ray) (:world/objects world))))


(defn shadowed? [world point]
  (let [{:world/keys [light objects]} world
        point-to-light-vec (tup/tup-sub (:light/position light) point)
        distance (tup/magnitude point-to-light-vec)
        ray (ray/ray point (tup/normalize point-to-light-vec))]
    (boolean
     (when-let [hit (intersection/hit (intersect-world world ray))]
       (< (:intersection/t hit) distance)))))

(declare color-at)

(defn- calc-reflected-color [world comps reflective remaining]
  (let [{:intersection/keys [reflectv over-point]} comps
        reflect-ray (ray/ray over-point reflectv)
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
       (calc-reflected-color world comps reflective remaining)))))

(defn- calc-refracted-color [world comps sin2-t n-ratio cos-i remaining]
  (let [{:intersection/keys [object eyev normalv under-point]} comps
        cos-t (Math/sqrt (- 1.0 sin2-t))
        direction (tup/tup-sub
                   (tup/tup-mul-scalar normalv (- (* n-ratio cos-i) cos-t))
                   (tup/tup-mul-scalar eyev n-ratio))
        refract-ray (ray/ray under-point direction)]
    (color/color-mul-scalar
     (color-at world refract-ray (dec remaining))
     (get-in object [:material :material/transparency]))))

(defn refracted-color
  ([world comps]
   (refracted-color world comps max-mutual-recursion-depth))
  
  ([world comps remaining]
   (let [{:intersection/keys [object n1 n2 eyev normalv under-point]} comps
         transparency (get-in object [:material :material/transparency])
         n-ratio (/ n1 n2)
         cos-i (tup/dot eyev normalv)
         sin2-t (* (* n-ratio n-ratio) (- 1.0 (* cos-i cos-i)))
         total-internal-reflection? (< 1.0 sin2-t)]
     (if (or (< remaining 1)
             total-internal-reflection?
             (fcmp/nearly-zero? transparency))
       color/black
       (calc-refracted-color world comps sin2-t n-ratio cos-i remaining)))))

(defn shade-hit
  ([world comps]
   (shade-hit world comps max-mutual-recursion-depth))
  ([world comps remaining]
   (let [{:intersection/keys [object point eyev normalv over-point]} comps
         shadowed (shadowed? world over-point)
         surface  (material/lighting
                   (:material object) object (:world/light world) over-point
                   eyev normalv shadowed)
         reflected (reflected-color world comps remaining)
         refracted (refracted-color world comps remaining)]
     (color/color-add surface reflected refracted))))

(defn color-at
  ([world ray]
   (color-at world ray max-mutual-recursion-depth))
  ([world ray remaining]
   (if-let [hit (intersection/hit (intersect-world world ray))]
     (shade-hit world (comps/prepare-computations hit ray) remaining)
     color/black)))
