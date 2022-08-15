(ns racertay.shape-test
  (:require [clojure.test :refer :all]
            [racertay.shape :refer :all]
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
      (is (material/material-eq? material/new-material (:material s))))))

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
          rot (xform/rotation-z (/ Math/PI 4))
          scale (xform/scaling 2 2 2)
          compound-op (matrix/mat-mul rot trans scale)
          s (shape/apply-transform (sphere) scale trans rot)]
      (is (matrix/mat-eq? compound-op (:transform s)))))

  (testing "A sphere's inverse-transform is updated after transfomations"
    (let [trans (xform/translation 2 3 4)
          rot (xform/rotation-z (/ Math/PI 4))
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
          rad-3-over-3 (/ (Math/sqrt 3) 3)
          n (shape/normal-at s (tup/point rad-3-over-3 rad-3-over-3 rad-3-over-3))]
      (is (tup/tup-eq? (tup/vect rad-3-over-3 rad-3-over-3 rad-3-over-3) n))))

  (testing "A normal is normalized by default"
    (let [s (sphere)
          rad-3-over-3 (/ (Math/sqrt 3) 3)
          n (shape/normal-at s (tup/point rad-3-over-3 rad-3-over-3 rad-3-over-3))]
      (is (tup/tup-eq? (tup/normalize n) n))))

  (testing "The normal of a translated sphere"
    (let [s (shape/apply-transform (sphere) (xform/translation 0 1 0))
          n (shape/normal-at s (tup/point 0 1.70711 -0.70711))]
      (is (tup/tup-eq? (tup/vect 0 0.70711 -0.70711) n))))

  (testing "The normal of a transfomed sphere"
    (let [xform (matrix/mat-mul
                 (xform/scaling 1 0.5 1) (xform/rotation-z (/ Math/PI 5)))
          s (shape/apply-transform (sphere) xform)
          rad-2-over-2 (/ (Math/sqrt 2) 2)
          n (shape/normal-at s (tup/point 0 rad-2-over-2 (- rad-2-over-2)))]
      (is (tup/tup-eq? (tup/vect 0 0.97014 -0.24254) n)))))

(deftest sphere-material-test
  (testing "A sphere's material can be changed"
    (let [m (assoc material/new-material :material/ambient  1.0)
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
