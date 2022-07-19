(ns racertay.color
  (:require [racertay.fcmp :refer :all]))

(defn color [r g b]
  {:red (double r)
   :green (double g)
   :blue (double b)})

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
