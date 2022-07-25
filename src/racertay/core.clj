(ns racertay.core
  (:require [clojure.java.io :as io]
            [racertay.tuple :refer :all]
            [racertay.canvas :refer :all]
            [racertay.transformations :refer :all]
            [racertay.matrix :refer :all]
            [racertay.color :refer [color]]
            [racertay.sphere :as sphere]
            [racertay.ray :as ray]
            [racertay.intersection :as intersect]
            [racertay.material :as material]
            [racertay.light :as light])
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

(defn red-circle-canvas [width]
  (let [canvas (canvas width width)
        xform (mat-mul
               (rotation-z (/ Math/PI 6))
               (scaling 1.3 0.3 1))
        s (sphere/apply-transform (sphere/sphere) xform)
        ray-source (point 0 0 -5)
        wall-z 10.0
        wall-size 10.0
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

(defn sphere-canvas [width]
  (let [canvas (canvas width width)
        sphere-color (color 1 1 0)
        scale (scaling 1.3 0.3 1)
        rot (rotation-z (/ Math/PI 6))
        m (material/assoc-color (material/material) sphere-color)
        s (-> (sphere/sphere)
              (sphere/assoc-material m)
              (sphere/apply-transform scale rot))
        ray-source (point 0 0 -5)
        wall-z 10.0
        wall-size 10.0
        pixel-size (/ wall-size width)
        half-wall (/ wall-size 2)
        light-color (color 1 1 1)
        light-pos (point -10 10 -10)
        light (light/point-light light-pos light-color)
        f (fn [canvas [x y]]
            (let [world-y (- half-wall (* pixel-size y))
                  world-x (+ (- half-wall) (* pixel-size x))
                  wall-position (point world-x world-y wall-z)
                  ray-direction (normalize (tup-sub wall-position ray-source))
                  r (ray/ray ray-source ray-direction)
                  xs (sphere/intersect s r)]
              (if-let [i (intersect/hit xs)]
                (let [point (ray/position r (intersect/t i))
                      normal (sphere/normal-at (intersect/object i) point)
                      eye (tup-neg (ray/direction r))
                      material (sphere/material (intersect/object i))
                      color (material/lighting material light point eye normal)]
                  (write-pixel canvas x y color))
                canvas)))]
    (reduce f canvas (canvas-indices canvas))))

(defn -main
  [& args]
  (let [filename "output3.ppm"
        ppm (canvas-to-p6-ppm (sphere-canvas 500))]
    (with-open [out (io/output-stream filename)]
      (.write out ppm))))
