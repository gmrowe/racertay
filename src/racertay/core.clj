(ns racertay.core
  (:require [clojure.java.io :as io]
            [racertay.tuple :refer :all]
            [racertay.canvas :refer :all]
            [racertay.transformations :refer :all]
            [racertay.matrix :refer :all]
            [racertay.color :refer [color]]
            [racertay.sphere :as sphere]
            [racertay.ray :as ray]
            [racertay.intersection :as intersect])
  (:gen-class))

(defn clock-face [clock-radius]
  (let [hours 12
        origin (point 0 0 0)
        noon (point 0 1 0)
        scale (scaling 1 clock-radius 1)
        rotation-angle (/ (* Math/PI 2) hours)
        rotate (fn [n] (rotation-z (* n rotation-angle)))]
    (map (fn [n]
           (mat-mul-tup (mat-mul (rotate n) scale (translation 0 1 0)) origin))
         (range 0 hours))))

(def black (color 0 0 0))
(def white (color 1 1 1))

(defn clock-canvas [width height clock-radius]
  (let [init-canvas (canvas width height white)
        canvas-translation (translation (/ width 2) (/ height 2) 0)
        canvas-space (map #(mat-mul-tup canvas-translation %) (clock-face clock-radius))]
    (reduce (fn [c p] (write-pixel c (int (x p)) (int (y p)) black)) init-canvas canvas-space)))

(defn canvas-indices [canvas]
  (for [y (range 0 (:height canvas))
        x (range 0 (:width canvas))]
    [x y]))

(def sphere-canvas
  (let [width 100
        canvas (canvas width width)
        s (sphere/sphere)
        ray-source (point 0 0 -5)
        wall-z 10.0
        wall-size 15.0
        pixel-size (/ wall-size width)
        half-wall (/ wall-size 2)
        sphere-color (color 1 0 0)
        f (fn [canvas [x y]]
            (let [world-y (- half-wall (* pixel-size y))
                  world-x (+ (- half-wall) (* pixel-size x))
                  wall-position (point world-x world-y wall-z)
                  ray-direction (normalize (tup-sub wall-position ray-source))
                  r (ray/ray ray-source ray-direction)
                  xs (sphere/intersect s r)]
              (if (intersect/hit xs)
                (write-pixel canvas x y sphere-color)
                canvas)))]
    (reduce f canvas (canvas-indices canvas))))

(defn -main
  [& args]
  (let [filename "output2.ppm"
        ppm (canvas-to-p6-ppm sphere-canvas)]
    (with-open [out (io/output-stream filename)]
      (.write out ppm))))
