(ns racertay.tuple
  (:require [racertay.fcmp :refer :all]))

(defn tuple [x y z w]
  [(double x) (double y) (double z) (double  w)])

(defn x [tup]
  (let [[x _ _ _] tup]
    x))

(defn y [tup]
  (let [[_ y _ _] tup]
    y))

(defn z [tup]
  (let [[_ _ z _] tup]
    z))

(defn w [tup]
  (let [[_ _ _ w] tup]
    w))

(defn vect? [tup]
  (nearly-eq? 0.0 (w tup)))

(defn point? [tup]
  (nearly-eq? 1.0 (w tup)))

(defn point [x y z]
  (tuple x y z 1.0))

(defn vect [x y z]
  (tuple x y z 0.0))

(defn tup-eq? [t1 t2]
  (every? identity (map nearly-eq? t1 t2)))

(defn tup-add
  ([t1] t1)
  ([t1 t2]
   (apply tuple (map + t1 t2)))
  ([t1 t2 & more]
   (reduce tup-add (tup-add t1 t2) more)))

(defn tup-sub
  ([t1]
   (let [zero (tuple 0 0 0 0)]
     (tup-sub zero t1)))
  ([t1 t2]
   (apply tuple (map - t1 t2)))
  ([t1 t2 & more]
   (reduce tup-sub (tup-sub t1 t2) more)))

(defn tup-neg [t]
  (tup-sub t))

(defn tup-mul-scalar [t scalar]
  (apply tuple (map * t (repeat scalar))))

(defn tup-div-scalar [t scalar]
  (tup-mul-scalar t (/ 1.0 scalar)))

(defn magnitude [v]
  (letfn [(square [x] (* x x))]
    (Math/sqrt (apply + (map square v)))))

(defn normalize [v]
  (let [m (magnitude v)]
    (apply tuple (map #(/ % m) v))))

(defn dot [v1 v2]
  (apply + (map * v1 v2)))

(defn cross [v1 v2]
  (vect (- (* (y v1) (z v2)) (* (z v1) (y v2)))
        (- (* (z v1) (x v2)) (* (x v1) (z v2)))
        (- (* (x v1) (y v2)) (* (y v1) (x v2)))))

(defn reflect [v n]
  (tup-sub v (tup-mul-scalar n (* 2 (dot v n)))))
