(ns racertay.transformations
  (:require [racertay.matrix :refer [mat4x4]]))

(defn translation [x y z]
  (mat4x4
   1 0 0 x
   0 1 0 y
   0 0 1 z
   0 0 0 1))

(defn scaling [x y z]
  (mat4x4
   x 0 0 0
   0 y 0 0
   0 0 z 0
   0 0 0 1))

(def x-reflection (scaling -1 1 1))

(def y-reflection (scaling 1 -1 1))

(def z-reflection (scaling 1 1 -1))

(defn rotation-x [rad]
  (let [c (Math/cos rad)
        s (Math/sin rad)]
    (mat4x4
     1 0    0  0
     0 c (- s) 0
     0 s    c  0
     0 0    0  1)))

(defn rotation-y [rad]
  (let [c (Math/cos rad)
        s (Math/sin rad)]
    (mat4x4
     c     0 s 0
     0     1 0 0
     (- s) 0 c 0
     0     0 0 1)))

(defn rotation-z [rad]
  (let [c (Math/cos rad)
        s (Math/sin rad)]
    (mat4x4
     c (- s) 0 0
     s    c  0 0
     0    0  1 0
     0    0  0 1)))

(defn shearing [xy xz yx yz zx zy]
  (mat4x4
   1  xy xz 0
   yx 1  yz 0
   zx zy 1  0
   0  0  0  1))
