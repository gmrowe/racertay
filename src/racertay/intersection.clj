(ns racertay.intersection
  (:require [clojure.math :as math]
            [racertay.ray :as ray]
            [racertay.tuple :as tup]
            [racertay.fcmp :as fcmp]))

(defn intersection [t obj]
  #:intersection{:t t
                 :object obj})

(def intersections vector)
(def empty-intersections (intersections))

(defn schlick [comps]
  (let [{:intersection/keys [eyev normalv n1 n2]} comps
        cos (tup/dot eyev normalv)
        n-ratio (/ n1 n2)
        sin2-t (* n-ratio n-ratio (- 1.0 (* cos cos)))
        cos-t (math/sqrt (- 1.0 sin2-t))
        cos-angle (if (< n2 n1) cos-t cos)
        r0 (math/pow (/ (- n1 n2) (+ n1 n2)) 2)]
    (if (< 1.0 sin2-t)
      1.0
      (+ r0 (* (- 1.0 r0) (math/pow (- 1.0 cos-angle) 5)) ))))

(defn hit [inters]
  (when-let [hits (seq (filter #(pos? (:intersection/t %)) inters))]
    (apply min-key :intersection/t hits)))
