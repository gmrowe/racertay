(ns racertay.color
  (:require [racertay.fcmp :refer :all]))

(defn color [r g b]
  {:red (double r)
   :green (double g)
   :blue (double b)})

(def black (color 0 0 0))
(def white (color 1 1 1))
(def red (color 1 0 0))
(def green (color 0 1 0))
(def blue (color 0 0 1))
(def aqua (color 0 1 1))
(def yellow (color 1 1 0))
(def fuchsia (color 1 0 1))
(def pink (color 1 0.71 0.77))
(def orange (color 1 0.65 0))
(def indigo (color 0.29 0 0.51))

(defn color-eq? [c1 c2]
  (and
   (nearly-eq? (:red c1) (:red c2))
   (nearly-eq? (:green c1) (:green c2))
   (nearly-eq? (:blue c1) (:blue c2))))

(defn color-add
  ([c] c)
  ([c1 c2]
   (color (+ (:red c1) (:red c2))
          (+ (:green c1) (:green c2))
          (+ (:blue c1) (:blue c2))))
  ([c1 c2 & more]
   (reduce color-add c1 (cons c2 more))))

(defn color-sub
  ([c]
   (let [black (color 0 0 0)]
     (color-sub black c)))
  ([c1 c2]
   (color (- (:red c1) (:red c2))
          (- (:green c1) (:green c2))
          (- (:blue c1) (:blue c2))))
  ([c1 c2 & more]
   (reduce color-sub c1 (cons c2 more))))

(defn color-mul-scalar [c scalar]
  (color (* (:red c) scalar)
         (* (:green c) scalar)
         (* (:blue c) scalar)))

(defn color-mul [c1 c2]
  (color (* (:red c1) (:red c2))
         (* (:green c1) (:green c2))
         (* (:blue c1) (:blue c2))))
