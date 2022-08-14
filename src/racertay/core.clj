(ns racertay.core
  (:require [clojure.java.io :as io]
            [racertay.tuple :as tup]
            [racertay.color :as col]
            [racertay.sphere :as sphere]
            [racertay.plane :as plane]
            [racertay.transformations :as xform]
            [racertay.world :as wor]
            [racertay.light :as light]
            [racertay.camera :as cam]
            [racertay.canvas :as canv]
            [racertay.protocols :as p]
            [racertay.pattern :as patt])
  (:gen-class))


(def floor
  (-> (plane/plane)
      (assoc-in [:material :material/color] (col/color 1 0.9 0.9))
      (assoc-in [:material :material/specular] 0)))

(def left-wall
  (-> (plane/plane)
      (p/apply-transform
       (xform/rotation-x (/ Math/PI 2))
       (xform/rotation-y (- (/ Math/PI 4)))
       (xform/translation 0 0 5))))

(defn chevron-pattern [color-a color-b]
  (let [angle (/ Math/PI 2)
        scale (xform/scaling 0.50 0.50 0.50)
        horizontal (xform/rotation-y angle)
        pattern (patt/stripe-pattern color-a color-b)]
    (p/apply-transform
     (patt/nested-checker-pattern
      (p/apply-transform pattern scale horizontal)
      (p/apply-transform pattern scale))
     (xform/rotation-y (/ Math/PI -4)))))

(def right-wall
  (-> (plane/plane)
      (p/apply-transform
       (xform/rotation-x (/ Math/PI 2))
       (xform/rotation-y (/ Math/PI 4))
       (xform/translation 0 0 5))
      (assoc-in [:material :material/pattern]
                (chevron-pattern col/orange col/green))))

(def middle-sphere
  (let [pattern (-> (patt/gradient-pattern col/violet col/beige)
                    (p/apply-transform (xform/scaling 0.2 0.2 0.2)
                                       (xform/rotation-z (/ Math/PI 3))))]
    (-> (sphere/sphere)
        (p/apply-transform (xform/translation -0.5 1 0.5))
        (assoc-in [:material :material/pattern] pattern)
        (assoc-in [:material :material/diffuse] 0.7)
        (assoc-in [:material :material/specular] 0.3))))

(def right-sphere
  (-> (sphere/sphere)
      (p/apply-transform
       (xform/scaling 0.5 0.5 0.5)
       (xform/translation 1.5 0.5 -0.5))
      (assoc-in [:material :material/color] col/dark-red)
      (assoc-in [:material :material/diffuse] 0.7)
      (assoc-in [:material :material/specular] 0.3)))

(def left-sphere
  (-> (sphere/sphere)
      (p/apply-transform
       (xform/scaling 0.33 0.33 0.33)
       (xform/translation -1.5 0.33 -0.75))
      (assoc-in [:material :material/color] col/gold)
      (assoc-in [:material :material/diffuse] 0.7)
      (assoc-in [:material :material/specular] 0.3)))

(def light
  (light/point-light
   (tup/point -10 10 -10) col/white))

(def width 800)
(def height 600)
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
  (let [canvas (cam/render camera world :report)]
    (write-canvas-to-ppm-file canvas "output.ppm")))
