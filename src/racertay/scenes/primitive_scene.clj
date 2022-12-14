(ns racertay.scenes.primitive-scene
  (:require [clojure.math :as math]
            [racertay.camera :as cam]
            [racertay.color :as col]
            [racertay.light :as light]
            [racertay.pattern :as patt]
            [racertay.shape :as shape]
            [racertay.transformations :as xform]
            [racertay.tuple :as tup]
            [racertay.world :as wor]))

(def floor
  (-> (shape/plane)
      (assoc-in [:material :material/color] (col/color 1 0.9 0.9))
      (assoc-in [:material :material/specular] 0)
      (assoc-in [:material :material/reflective] 0.85)))

(def left-wall
  (-> (shape/plane)
      (xform/apply-transform
       (xform/rotation-x (/ math/PI 2))
       (xform/rotation-y (- (/ math/PI 4)))
       (xform/translation 0 0 5))))

(def right-wall
  (-> (shape/plane)
      (xform/apply-transform
       (xform/rotation-x (/ math/PI 2))
       (xform/rotation-y (/ math/PI 4))
       (xform/translation 0 0 5))
      (assoc-in [:material :material/pattern]
                (patt/chevron-pattern col/orange col/green))))

(def left-sphere
  (-> (shape/sphere)
      (xform/apply-transform
       (xform/scaling 0.33 0.33 0.33)
       (xform/translation -1.5 0.33 -0.75))
      (assoc-in [:material :material/color] col/dark-blue)
      (assoc-in [:material :material/diffuse] 0.7)
      (assoc-in [:material :material/specular] 0.3)
      (assoc-in [:materail :material/reflective] 1.0)))

(def middle-sphere
  (let [pattern (-> (patt/stripe-pattern col/violet col/white)
                    (xform/apply-transform (xform/scaling 0.2 0.2 0.2)
                                       (xform/rotation-z (/ math/PI 3))))]
    (-> (shape/sphere)
        (xform/apply-transform (xform/translation -0.5 1 0.5))
        (assoc-in [:material :material/pattern] pattern)
        (assoc-in [:material :material/diffuse] 0.7)
        (assoc-in [:material :material/specular] 0.3))))

(def right-cube
  (-> (shape/cube)
      (xform/apply-transform
       (xform/scaling 0.25 1.5 0.25)
       (xform/translation 1.5 0.5 -0.5))
      (assoc-in [:material :material/color] col/black)
      (assoc-in [:material :material/diffuse] 0.5)
      (assoc-in [:material :material/ambient] 0.4)
      (assoc-in [:material :material/specular] 0.4)
      (assoc-in [:material :material/transparency] 0.91)
      (assoc-in [:material :material/refractive-index] 2.41)
      (assoc-in [:material :material/reflective] 0.7)))

(def cylinder
  (-> (shape/cylinder 1.6 0.5 :closed)
      (xform/apply-transform
       (xform/scaling 0.5 1.0 0.5)
       (xform/translation 0.44 -0.8 -1.5))
      (assoc-in [:material :material/color] col/light-blue)))

(def cone
  (-> (shape/cone -1 0 :closed)
      (xform/apply-transform
       (xform/translation -0.33 1 -0.7))
      (assoc-in [:material :material/color] col/dark-red)
      (assoc-in [:materail :materail/ambient] 0.7)))

(def light
  (light/point-light
   (tup/point -10 10 -10) col/white))

(def field-of-view (/ math/PI 3))
(def camera-location (tup/point 0 1.5 -5))
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
      (update :world/objects conj floor)
      (update :world/objects conj left-wall)
      (update :world/objects conj right-wall)
      (update :world/objects conj middle-sphere)
      (update :world/objects conj right-cube)
      (update :world/objects conj left-sphere)
      (update :world/objects conj cylinder)
      (update :world/objects conj cone)))

(defn scene
  [width height]
  {:camera (camera width height)
   :world world})
