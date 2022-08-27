(ns racertay.shape-test
  (:require [clojure.test :refer :all]
            [racertay.shape :refer :all]
            [clojure.math :as math]
            [racertay.transformations :as xform]
            [racertay.ray :as ray]
            [racertay.tuple :as tup]
            [racertay.fcmp :as fcmp]
            [racertay.intersection :as inter]
            [racertay.matrix :as matrix]
            [racertay.material :as material]
            [racertay.shape :as shape]
            [racertay.protocols :as p]))

(deftest sphere-creation-test
  (testing "A sphere must always return a unique value"
    (let [s1 (sphere)
          s2 (sphere)]
      (is (= s1 s1))
      (is (not= s1 s2))))

  (testing "A sphere has a default material"
    (let [s (sphere)]
      (is (material/material-eq? material/default-material (:material s))))))

(deftest sphere-ray-intersection-test
  (testing "A ray which intersects a sphere at two points"
    (let [r (ray/ray (tup/point 0 0 -5) (tup/vect 0 0 1))
          s (sphere)
          xs (shape/intersect s r)]
      (is (= 2 (count xs)))
      (is (fcmp/nearly-eq? 4.0 (:intersection/t (nth xs 0))))
      (is (fcmp/nearly-eq? 6.0 (:intersection/t (nth xs 1))))))

  (testing "A ray which intersects a sphere at a tangent"
    (let [r (ray/ray (tup/point 0 1 -5) (tup/vect 0 0 1))
          s (sphere)
          xs (shape/intersect s r)]
      (is (= 2 (count xs)))
      (is (fcmp/nearly-eq? 5.0 (:intersection/t (nth xs 0))))
      (is (fcmp/nearly-eq? 5.0 (:intersection/t (nth xs 1))))))

  (testing "A ray which misses a sphere"
    (let [r (ray/ray (tup/point 0 2 -5) (tup/vect 0 0 1))
          s (sphere)
          xs (shape/intersect s r)]
      (is (zero? (count xs)))))

  (testing "A ray which originates inside a sphere"
    (let [r (ray/ray (tup/point 0 0 0) (tup/vect 0 0 1))
          s (sphere)
          xs (shape/intersect s r)]
      (is (= 2 (count xs)))
      (is (fcmp/nearly-eq? -1.0 (:intersection/t (nth xs 0))))
      (is (fcmp/nearly-eq? 1.0 (:intersection/t (nth xs 1))))))

  (testing "A sphere that is completly behind a ray"
    (let [r (ray/ray (tup/point 0 0 5) (tup/vect 0 0 1))
          s (sphere)
          xs (shape/intersect s r)]
      (is (= 2 (count xs)))
      (is (fcmp/nearly-eq? -6.0 (:intersection/t (nth xs 0))))
      (is (fcmp/nearly-eq? -4.0 (:intersection/t (nth xs 1))))))

  (testing "intersect sets the object on the intersecton"
    (let [r (ray/ray (tup/point 0 0 -5) (tup/vect 0 0 1))
          s (sphere)
          xs (shape/intersect s r)]
      (is (= 2 (count xs)))
      (is (= s (:intersection/object (nth xs 0))))
      (is (= s (:intersection/object (nth xs 1))))))

  (testing "Intersecting a scaled sphere with a ray"
    (let [r (ray/ray (tup/point 0 0 -5) (tup/vect 0 0 1))
          s (shape/apply-transform (sphere) (xform/scaling 2 2 2))
          xs (shape/intersect s r)]
      (is (= 2 (count xs)))
      (is (fcmp/nearly-eq? 3 (:intersection/t (nth xs 0))))
      (is (fcmp/nearly-eq? 7 (:intersection/t (nth xs 1))))))

  (testing "Intersecting a translated sphere with a ray"
    (let [r (ray/ray (tup/point 0 0 -5) (tup/vect 0 0 1))
          s (shape/apply-transform (sphere) (xform/translation 5 0 0))
          xs (shape/intersect s r)]
      (is (zero? (count xs))))))

(deftest sphere-transformation-test
  (testing "A sphere has a default transformation"
    (let [s (sphere)]
      (is (matrix/mat-eq? matrix/identity-matrix (:transform s)))))

  (testing "A sphere's transformation can be changed"
    (let [t (xform/translation 2 3 4)
          s (shape/apply-transform (sphere) t)]
      (is (matrix/mat-eq? t (:transform s)))))

  (testing "A sphere can incorporate multiple transfomations"
    (let [trans (xform/translation 2 3 4)
          rot (xform/rotation-z (/ math/PI 4))
          scale (xform/scaling 2 2 2)
          compound-op (matrix/mat-mul rot trans scale)
          s (shape/apply-transform (sphere) scale trans rot)]
      (is (matrix/mat-eq? compound-op (:transform s)))))

  (testing "A sphere's inverse-transform is updated after transfomations"
    (let [trans (xform/translation 2 3 4)
          rot (xform/rotation-z (/ math/PI 4))
          scale (xform/scaling 2 2 2)
          compound-op (matrix/mat-mul rot trans scale)
          s (shape/apply-transform (sphere) scale trans rot)]
      (is (matrix/mat-eq? (matrix/inverse compound-op)
                          (:inverse-transform s))))))

(deftest sphere-normal-test
  (testing "A normal of a sphere on the x-axis"
    (let [s (sphere)
          n (shape/normal-at s (tup/point 1 0 0))]
      (is (tup/tup-eq? (tup/vect 1 0 0) n))))

  (testing "A normal of a sphere on the y-axis"
    (let [s (sphere)
          n (shape/normal-at s (tup/point 0 1 0))]
      (is (tup/tup-eq? (tup/vect 0 1 0) n))))

  (testing "A normal of a sphere on the z-axis"
    (let [s (sphere)
          n (shape/normal-at s (tup/point 0 0 1))]
      (is (tup/tup-eq? (tup/vect 0 0 1) n))))

  (testing "A normal of a sphere at a nonaxial point"
    (let [s (sphere)
          rad-3-over-3 (/ (math/sqrt 3) 3)
          n (shape/normal-at s (tup/point rad-3-over-3 rad-3-over-3 rad-3-over-3))]
      (is (tup/tup-eq? (tup/vect rad-3-over-3 rad-3-over-3 rad-3-over-3) n))))

  (testing "A normal is normalized by default"
    (let [s (sphere)
          rad-3-over-3 (/ (math/sqrt 3) 3)
          n (shape/normal-at s (tup/point rad-3-over-3 rad-3-over-3 rad-3-over-3))]
      (is (tup/tup-eq? (tup/normalize n) n))))

  (testing "The normal of a translated sphere"
    (let [s (shape/apply-transform (sphere) (xform/translation 0 1 0))
          n (shape/normal-at s (tup/point 0 1.70711 -0.70711))]
      (is (tup/tup-eq? (tup/vect 0 0.70711 -0.70711) n))))

  (testing "The normal of a transfomed sphere"
    (let [xform (matrix/mat-mul
                 (xform/scaling 1 0.5 1) (xform/rotation-z (/ math/PI 5)))
          s (shape/apply-transform (sphere) xform)
          rad-2-over-2 (/ (math/sqrt 2) 2)
          n (shape/normal-at s (tup/point 0 rad-2-over-2 (- rad-2-over-2)))]
      (is (tup/tup-eq? (tup/vect 0 0.97014 -0.24254) n)))))

(deftest sphere-material-test
  (testing "A sphere's material can be changed"
    (let [m (assoc material/default-material :material/ambient  1.0)
          s (assoc (sphere) :material m)]
      (is (material/material-eq? m (:material s))))))

(deftest plane-normal-test
  (testing "The normal of a plane is contant everywhere"
    (let [p (plane)
          n1 (p/local-normal-at p (tup/point 0 0 0))
          n2 (p/local-normal-at p (tup/point 10 0 -10))
          n3 (p/local-normal-at p (tup/point -5 0 150))]
      (is (tup/tup-eq? n1 n2 n1)))))

(deftest plane-intersect-test
  (testing "A ray parallel to the plane never intersects"
    (let [p (plane)
          r (ray/ray (tup/point 0 10 0) (tup/vect 0 0 1))
          xs (p/local-intersect p r)]
      (is (not (seq xs)))))

  (testing "A coplaner ray never intersects"
    (let [p (plane)
          r (ray/ray (tup/point 0 0 0) (tup/vect 0 0 1))
          xs (p/local-intersect p r)]
      (is (not (seq xs)))))

  (testing "A ray can intersect a plane form above"
    (let [p (plane)
          r (ray/ray (tup/point 0 -1 0) (tup/vect 0 1 0))
          xs (p/local-intersect p r)]
      (is (= 1 (count xs)))
      (is (fcmp/nearly-eq? 1 (:intersection/t (nth xs 0))))
      (is (= p (:intersection/object (nth xs 0))))))

  (testing "A ray can intersect a plane form below"
    (let [p (plane)
          r (ray/ray (tup/point 0 1 0) (tup/vect 0 -1 0))
          xs (p/local-intersect p r)]
      (is (= 1 (count xs)))
      (is (fcmp/nearly-eq? 1 (:intersection/t (nth xs 0))))
      (is (= p (:intersection/object (nth xs 0)))))))

(deftest cube-local-intersect-test
  (testing "Ray intersects cube"
    (letfn [ (ray-intersects-cube-test
              [ray-origin ray-direction expected-t1 expected-t2]
              (let [c (shape/cube)
                    r (ray/ray ray-origin ray-direction)
                    xs (p/local-intersect c r)]
                (is (fcmp/nearly-eq? expected-t1 (:intersection/t (nth xs 0)))
                    "The 1st intercept.")
                (is (fcmp/nearly-eq? expected-t2 (:intersection/t (nth xs 1)))
                    "The 2nd intercept.")))]
      (testing "in the +x face"
        (ray-intersects-cube-test (tup/point 5 0.5 0) (tup/vect -1 0 0) 4 6))
      (testing "in the -x face"
        (ray-intersects-cube-test (tup/point -5 0.5 0) (tup/vect 1 0 0) 4 6))
      (testing "in the +y face"
        (ray-intersects-cube-test (tup/point 0.5 5 0) (tup/vect 0 -1 0) 4 6))
      (testing "in the -y face"
        (ray-intersects-cube-test (tup/point 0.5 -5 0) (tup/vect 0 1 0) 4 6))
      (testing "in the +z face"
        (ray-intersects-cube-test (tup/point 0.5 0 -5) (tup/vect 0 0 1) 4 6))
      (testing "in the -z face"
        (ray-intersects-cube-test (tup/point 0.5 0 5) (tup/vect 0 0 -1) 4 6))
      (testing "from inside the cube"
        (ray-intersects-cube-test (tup/point 0 0.5 0) (tup/vect 0 0 1) -1 1))))

  (testing "Ray misses cube"
    (letfn [(ray-misses-cube-test
              [ray-origin ray-direction]
              (let [c (shape/cube)
                    r (ray/ray ray-origin ray-direction)
                    xs (p/local-intersect c r)]
                (is (zero? (count xs)))))]
      (testing "from the -x direction"
        (ray-misses-cube-test (tup/point -2 0 0) (tup/vect 0.2673 0.5345 0.8018)))
      (testing "from the -y direction"
        (ray-misses-cube-test (tup/point 0 -2 0) (tup/vect 0.8018 0.2673 0.5345)))
      (testing "from the -z direction"
        (ray-misses-cube-test (tup/point 0 0 -2) (tup/vect 0.5345 0.8018 0.2673)))
      (testing "parallel to the +x face"
        (ray-misses-cube-test (tup/point 2 0 2) (tup/vect 0 0 -1)))
      (testing "parallel to the +z face"
        (ray-misses-cube-test (tup/point 0 2 2) (tup/vect 0 -1 0)))
      (testing "parellel to the +y face"
        (ray-misses-cube-test (tup/point 2 2 0) (tup/vect -1 0 0))))))

(deftest cube-local-normal-at-test
  (testing "Normal on surface"
    (letfn [(normal-on-cube-surface-test
              [point expected-normal]
              (is (tup/tup-eq?
                   expected-normal (p/local-normal-at (shape/cube) point))))]
      (testing "on +x face"
        (normal-on-cube-surface-test (tup/point 1 0.5 -0.8) (tup/vect 1 0 0)))
      (testing "on -x face"
        (normal-on-cube-surface-test (tup/point -1 0.2 -0.9) (tup/vect -1 0 0)))
      (testing "on +y face"
        (normal-on-cube-surface-test (tup/point -0.4 1 -0.1) (tup/vect 0 1 0)))
      (testing "on -y face"
        (normal-on-cube-surface-test (tup/point -0.26 -1 0.16) (tup/vect 0 -1 0)))
      (testing "on +z face"
        (normal-on-cube-surface-test (tup/point 0.4 -0.31 1) (tup/vect 0 0 1)))
      (testing "on -z face"
        (normal-on-cube-surface-test (tup/point 0.042 0.7 -1) (tup/vect 0 0 -1)))
      (testing "on +x+y+z corner"
        (normal-on-cube-surface-test (tup/point 1 1 1) (tup/vect 1 0 0)))
      (testing "on -x-y-z corner"
        (normal-on-cube-surface-test (tup/point -1 -1 -1) (tup/vect -1 0 0))))))

(deftest cube-creation-test
  (testing "A cube has a default transform"
    (is (matrix/mat-eq? matrix/identity-matrix (:transform (shape/cube)))))

  (testing "A cube has default material"
    (is (material/material-eq? material/default-material (:material (shape/cube))))))

(deftest cylinder-local-intersect-test
  (testing "A ray that misses a cylinder"
    (letfn [(ray-misses-cylinder-test
              [origin direction]
              (let [cyl (shape/cylinder)
                    d (tup/normalize direction)
                    xs (p/local-intersect cyl (ray/ray origin d))]
                (is (zero? (count xs)))))]
      (testing "on outside surface - parallel to y-axis"
        (ray-misses-cylinder-test (tup/point 1 0 0) (tup/vect 0 1 0)))
      (testing "inside cylineder - parallel to y-axis"
        (ray-misses-cylinder-test (tup/point 0 0 0) (tup/vect 0 1 0)))
      (testing "outside cylinder - askew from all axes"
        (ray-misses-cylinder-test (tup/point 0 0 -5) (tup/vect 1 1 1)))))

  (testing "A ray that strikes a cylinder"
    (letfn [(ray-strikes-cylinder-test
              [origin direction expected-t1 expected-t2]
              (let [cyl (shape/cylinder)
                    d (tup/normalize direction)
                    xs (p/local-intersect cyl (ray/ray origin d))]
                (is (fcmp/nearly-eq?
                     expected-t1 (:intersection/t (nth xs 0))) "The first intersect")
                (is (fcmp/nearly-eq?
                     expected-t2 (:intersection/t (nth xs 1))) "The second intersect")))]
      (testing "on a tangent"
        (ray-strikes-cylinder-test (tup/point 1 0 -5) (tup/vect 0 0 1) 5 5))
      (testing "perpendicularly through center"
        (ray-strikes-cylinder-test (tup/point 0 0 -5) (tup/vect 0 0 1) 4 6))
      (testing "at an angle"
        (ray-strikes-cylinder-test (tup/point 0.5 0 -5) (tup/vect 0.1 1 1) 6.80798 7.08872)))))

(deftest cylinder-local-normal-test
  (testing "Normal vector on a cylinder"
    (letfn [(normal-on-cylinder-surface-test
              [point expected-normal]
              (is (tup/tup-eq?
                   expected-normal (p/local-normal-at (shape/cylinder) point))))]
      (testing "at a +x point"
        (normal-on-cylinder-surface-test (tup/point 1 0 0) (tup/vect 1 0 0)))
      (testing "at a -z point"
        (normal-on-cylinder-surface-test (tup/point 0 5 -1) (tup/vect 0 0 -1)))
      (testing "at a +z point"
        (normal-on-cylinder-surface-test (tup/point 0 -2 1) (tup/vect 0 0 1)))
      (testing "at a -x point"
        (normal-on-cylinder-surface-test (tup/point -1 1 0) (tup/vect -1 0 0))))))
