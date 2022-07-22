(ns racertay.core
  (:require [racertay.tuple :refer [point x y]]
            [racertay.canvas :refer :all]
            [racertay.transformations :refer :all]
            [racertay.matrix :refer :all]
            [racertay.color :refer [color]])
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

(defn -main
  [& args]
  (let [filename "output.ppm"]
    (spit filename (canvas-to-ppm (clock-canvas 200 200 50)))))
