(ns racertay.tuple
  (:require [racertay.fcmp :refer :all]))

(defn tuple [x y z w]
  {:x x
   :y y
   :z z
   :w w})

(defn vect? [tup]
  (nearly-eq? 0.0 (:w tup)))

(defn point? [tup]
  (nearly-eq? 1.0 (:w tup)))

(defn point [x y z]
  (tuple x y z 1.0))

(defn vect [x y z]
  (tuple x y z 0.0))

(defn tup-eq? [t1 t2]
  (and
   (nearly-eq? (:x t1) (:x t2))
   (nearly-eq? (:y t1) (:y t2))
   (nearly-eq? (:z t1) (:z t2))
   (nearly-eq? (:w t1) (:w t2))))

(defn tup-add
  ([t1] t1)
  ([t1 t2]
   (tuple (+ (:x t1) (:x t2))
          (+ (:y t1) (:y t2))
          (+ (:z t1) (:z t2))
          (+ (:w t1) (:w t2))))
  ([t1 t2 & more]
   (reduce tup-add (tup-add t1 t2) more)))

(defn tup-sub
  ([t1]
   (let [zero (tuple 0 0 0 0)]
     (tup-sub zero t1)))
  ([t1 t2]
   (tuple (- (:x t1) (:x t2))
          (- (:y t1) (:y t2))
          (- (:z t1) (:z t2))
          (- (:w t1) (:w t2))))
  ([t1 t2 & more]
   (reduce tup-sub (tup-sub t1 t2) more)))

(defn tup-neg [t]
  (tup-sub t))

(defn tup-mul-scalar [t scalar]
  (tuple (* (:x t) scalar)
         (* (:y t) scalar)
         (* (:z t) scalar)
         (* (:w t) scalar)))

(defn tup-div-scalar [t scalar]
  (tup-mul-scalar t (/ 1.0 scalar)))

(defn magnitude [v]
  (letfn [(square [x] (* x x))]
    (Math/sqrt (+ (square (:x v))
                  (square (:y v))
                  (square (:z v))
                  (square (:w v))))))

(defn normalize [v]
  (let [m (magnitude v)]
    (tuple (/ (:x v) m)
           (/ (:y v) m)
           (/ (:z v) m)
           (/ (:w v) m))))

(defn dot [v1 v2]
  (+ (* (:x v1) (:x v2))
     (* (:y v1) (:y v2))
     (* (:z v1) (:z v2))
     (* (:w v1) (:w v2))))

(defn cross [v1 v2]
  (vect (- (* (:y v1) (:z v2)) (* (:z v1) (:y v2)))
        (- (* (:z v1) (:x v2)) (* (:x v1) (:z v2)))
        (- (* (:x v1) (:y v2)) (* (:y v1) (:x v2)))))
