(ns racertay.transformations
  (:require [racertay.matrix :as mat]
            [racertay.tuple :as tup]
            [clojure.math :as math]))

(defn translation [x y z]
  (mat/mat4x4
   1 0 0 x
   0 1 0 y
   0 0 1 z
   0 0 0 1))

(defn scaling [x y z]
  (mat/mat4x4
   x 0 0 0
   0 y 0 0
   0 0 z 0
   0 0 0 1))

(def x-reflection (scaling -1 1 1))

(def y-reflection (scaling 1 -1 1))

(def z-reflection (scaling 1 1 -1))

(defn rotation-x [rad]
  (let [c (math/cos rad)
        s (math/sin rad)]
    (mat/mat4x4
     1 0    0  0
     0 c (- s) 0
     0 s    c  0
     0 0    0  1)))

(defn rotation-y [rad]
  (let [c (math/cos rad)
        s (math/sin rad)]
    (mat/mat4x4
     c     0 s 0
     0     1 0 0
     (- s) 0 c 0
     0     0 0 1)))

(defn rotation-z [rad]
  (let [c (math/cos rad)
        s (math/sin rad)]
    (mat/mat4x4
     c (- s) 0 0
     s    c  0 0
     0    0  1 0
     0    0  0 1)))

(defn shearing [xy xz yx yz zx zy]
  (mat/mat4x4
   1  xy xz 0
   yx 1  yz 0
   zx zy 1  0
   0  0  0  1))

(defn orientation-matrix [forward left true-up]
  (mat/mat4x4
   (tup/x left)        (tup/y left)        (tup/z left)        0
   (tup/x true-up)     (tup/y true-up)     (tup/z true-up)     0
   (- (tup/x forward)) (- (tup/y forward)) (- (tup/z forward)) 0
   0                   0                   0                   1))

(defn view-transform [from to up]
  (let [forward (tup/normalize (tup/tup-sub to from))
        left (tup/cross forward (tup/normalize up))
        true-up (tup/cross left forward)]
    (mat/mat-mul
     (orientation-matrix forward left true-up)
     (translation (- (tup/x from)) (- (tup/y from)) (- (tup/z from))))))

(defn apply-transform
  ([obj xform]
   (let [updated (update obj :transform (partial mat/mat-mul xform))]
     (assoc updated :inverse-transform (mat/inverse (:transform updated)))))
  
  ([obj xform & more]
   (reduce apply-transform obj (cons xform more))))
