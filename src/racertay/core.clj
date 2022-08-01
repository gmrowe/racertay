(ns racertay.core
  (:require [clojure.java.io :as io]
            [racertay.tuple :as tup]
            [racertay.color :as col]
            [racertay.sphere :as sphere]
            [racertay.transformations :as xform]
            [racertay.world :as wor]
            [racertay.light :as light]
            [racertay.camera :as cam]
            [racertay.canvas :as canv])
  (:gen-class))


(def floor
  (-> (sphere/sphere)
      (sphere/apply-transform (xform/scaling 10 0.01 10))
      (assoc-in [:material :material/color] (col/color 1 0.9 0.9))
      (assoc-in [:material :material/specular] 0)))

(def left-wall
  (-> (sphere/sphere)
      (sphere/apply-transform
       (xform/scaling 10 0.01 10)
       (xform/rotation-x (/ Math/PI 2))
       (xform/rotation-y (- (/ Math/PI 4)))
       (xform/translation 0 0 5))
      (assoc :material (:material floor))))

(def right-wall
  (-> (sphere/sphere)
      (sphere/apply-transform
       (xform/scaling 10 0.01 10)
       (xform/rotation-x (/ Math/PI 2))
       (xform/rotation-y (/ Math/PI 4))
       (xform/translation 0 0 5))
      (assoc :material (:material floor))))

(def middle-sphere
  (-> (sphere/sphere)
      (sphere/apply-transform
       (xform/translation -0.5 1 0.5))
      (assoc-in [:material :material/color] (col/color 0.1 1 0.5))
      (assoc-in [:material :material/diffuse] 0.7)
      (assoc-in [:material :material/specular] 0.3)))

(def right-sphere
  (-> (sphere/sphere)
      (sphere/apply-transform
       (xform/scaling 0.5 0.5 0.5)
       (xform/translation 1.5 0.5 -0.5))
      (assoc-in [:material :material/color] (col/color 0.5 1 0.1))
      (assoc-in [:material :material/diffuse] 0.7)
      (assoc-in [:material :material/specular] 0.3)))

(def left-sphere
  (-> (sphere/sphere)
      (sphere/apply-transform
       (xform/scaling 0.33 0.33 0.33)
       (xform/translation -1.5 0.33 -0.75))
      (assoc-in [:material :material/color] (col/color 1 0.8 0.1))
      (assoc-in [:material :material/diffuse] 0.7)
      (assoc-in [:material :material/specular] 0.3)))

(def light
  (light/point-light
   (tup/point -10 10 -10) (col/color 1 1 1)))

(def width 600)
(def height 450)
(def field-of-view (/ Math/PI 3))
(def camera-location (tup/point 0 1.5 -5))
(def canvas-location (tup/point 0 1 0))
(def up (tup/vect 0 1 0))

(def camera
  (-> (cam/camera width height field-of-view)
      (cam/apply-transform
       (xform/view-transform camera-location canvas-location up))))

(def world
  (-> wor/empty-world
      (assoc :world/light light)
      (update :world/objects conj floor)
      (update :world/objects conj left-wall)
      (update :world/objects conj right-wall)
      (update :world/objects conj middle-sphere)
      (update :world/objects conj right-sphere)
      (update :world/objects conj left-sphere)))

(defn write-canvas-to-ppm-file [canvas filename]
  (let [ppm-bytes (canv/canvas-to-p6-ppm canvas)]
    (with-open [out (io/output-stream filename)]
      (.write out ppm-bytes))))

(defn -main [& args]
  (let [canvas (cam/render camera world)]
    (write-canvas-to-ppm-file canvas "output.ppm")))


