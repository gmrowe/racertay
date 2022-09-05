(ns racertay.scenes.cone
  (:require [clojure.math :as math]
            [racertay.camera :as cam]
            [racertay.color :as col]
            [racertay.light :as light]
            [racertay.pattern :as patt]
            [racertay.shape :as shape]
            [racertay.transformations :as xform]
            [racertay.tuple :as tup]
            [racertay.world :as wor]))

(def cone-color (col/hex->color 0x754527))

(def cone
  (-> (shape/cone 0 1 :open)
      (xform/apply-transform
       (xform/scaling 0.7 1.9 0.7)
       (xform/translation 0 -0.6 0))
      (assoc-in [:material :material/color] cone-color)
      (assoc-in [:material :material/ambient] 0.4)
      (assoc-in [:material :material/specular] 0.0)))

(def ice-cream-color (col/hex->color 0xFFFDD0))

(def ice-cream
  (-> (shape/sphere)
      (xform/apply-transform
       (xform/scaling 0.69 0.71 0.70)
       (xform/translation 0 1.45 0))
      (assoc-in [:material :material/specular] 0.05)
      (assoc-in [:material :material/color] ice-cream-color)))

(def light
  (light/point-light
   (tup/point -10 10 -10) col/white))

(def field-of-view (/ math/PI 3))
(def camera-location (tup/point 0 1.9 -5))
(def canvas-location (tup/point 0 1 0))
(def up (tup/vect 0 1 0))

(defn camera
  [width height]
  (-> (cam/camera width height field-of-view)
      (xform/apply-transform
       (xform/view-transform camera-location canvas-location up))))

(def world
  (-> wor/empty-world
      (assoc :world/light light)
      (update :world/objects conj cone)
      (update :world/objects conj ice-cream)))

(defn scene
  [width height]
  {:camera (camera width height)
   :world world})
