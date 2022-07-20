(ns racertay.core
  (:require [clojure.string :as s]
            [racertay.tuple :refer :all]
            [racertay.canvas :refer :all]
            [racertay.color :refer [color]])
  (:gen-class))

(defn projectile [position velocity]
  {:position position
   :velocity velocity})

(defn environment [gravity wind]
  {:gravity gravity
   :wind wind})

(defn tick [env proj]
  (let [new-pos (tup-add (:position proj) (:velocity proj))
        new-vel (tup-add (tup-add (:velocity proj) (:gravity env))
                         (:wind env))]
    (projectile new-pos new-vel)))

(defn flight-path [env proj]
  (take-while #(> (y %) 0)
              (map :position (iterate (partial tick env) proj))))


(def flight-canvas
  (let [start-pos (point 0 1 0)
        start-direction (normalize (vect 1.0 2.3 0))
        start-speed 7.5
        velocity (tup-mul-scalar start-direction start-speed)
        gravity (vect 0 -0.1 0)
        wind (vect -0.01 0 0)
        canvas-width 400
        canvas-height 300
        projectile-color (color 1.0 1.0 0.0)
        flight (flight-path (environment gravity wind)
                            (projectile start-pos velocity))]
    (reduce (fn [c pos]
              (write-pixel c
                           (int (x pos))
                           (int (- (:height c) (:y pos)))
                           projectile-color))
            (canvas canvas-width canvas-height)
            flight)))

(defn -main
  [& args]
  (let [filename "output.ppm"]
    (spit filename (canvas-to-ppm flight-canvas))))
