(ns racertay.transformations-test
  (:require [clojure.test :refer :all]
            [racertay.transformations :refer :all]
            [clojure.math :as math]
            [racertay.tuple :as tup]
            [racertay.matrix :as mat]))

(deftest translation-test
  (testing "Multiplying a translation matrix by a point moves the point"
    (let [transform (translation 5 -3 2)
          p (tup/point -3 4 5)]
      (is (tup/tup-eq? (tup/point 2 1 7) (mat/mat-mul-tup transform p)))))

  (testing "Multiplying inverse of a translation matrix by a point moves point in reverse"
    (let [transform (translation 5 -3 2)
          p (tup/point -3 4 5)]
      (is (tup/tup-eq? (tup/point -8 7 3) (mat/mat-mul-tup (mat/inverse transform) p)))))

  (testing "Multiplying a translation matix by a vector does not change the vector"
    (let [transform (translation 5 -3 2)
          v (tup/vect -3 4 5)]
      (is (tup/tup-eq? v (mat/mat-mul-tup transform v))))))

(deftest scaling-test
  (testing "Multiplying a scaling matrix by a point scales the point"
    (let [transform (scaling 2 3 4)
          p (tup/point -4 6 8)]
      (is (tup/tup-eq? (tup/point -8 18 32) (mat/mat-mul-tup transform p)))))

  (testing "Multiplying scaling matrix by a vector changes its length"
    (let [transform (scaling 2 3 4)
          v (tup/vect -4 6 8)]
      (is (tup/tup-eq? (tup/vect -8 18 32) (mat/mat-mul-tup transform v)))))

  (testing "Multiplying inverse of a scaling matrix by a vector changes its length"
    (let [transform (mat/inverse (scaling 2 3 4))
          v (tup/vect -4 6 8)]
      (is (tup/tup-eq? (tup/vect -2 2 2) (mat/mat-mul-tup transform v)))))

  (testing "Reflection is scaling by a negative value"
    (let [transform (scaling -1 1 1)
          p (tup/point 2 3 4)]
      (is (tup/tup-eq? (tup/point -2 3 4) (mat/mat-mul-tup transform p)))
      (is (tup/tup-eq? (tup/point -2 3 4) (mat/mat-mul-tup x-reflection p))))))


(def rad2-over-2 (/ (math/sqrt 2) 2))
(def quarter-pi (/ math/PI 4))
(def half-pi (/ math/PI 2))

(deftest rotation-test
  (testing "A point can be rotated about the x-axis"
    (let [eighth-rot (rotation-x quarter-pi)
          quarter-rot (rotation-x half-pi)
          p (tup/point 0 1 0)]
      (is (tup/tup-eq? (tup/point 0 rad2-over-2 rad2-over-2) (mat/mat-mul-tup eighth-rot p)))
      (is (tup/tup-eq? (tup/point 0 0 1) (mat/mat-mul-tup quarter-rot p)))))

  (testing "The inverse of a rotation rotates in the opposite direction"
    (let [eighth-rot (rotation-x quarter-pi)
          quarter-rot (rotation-x half-pi)
          p (tup/point 0 1 0)]
      (is (tup/tup-eq? (tup/point 0 rad2-over-2 (- rad2-over-2))
                   (mat/mat-mul-tup (mat/inverse eighth-rot) p)))
      (is (tup/tup-eq? (tup/point 0 0 -1) (mat/mat-mul-tup (mat/inverse quarter-rot) p)))))

  (testing "A point can be rotated about the y-axis"
    (let [eight-rot (rotation-y quarter-pi)
          quarter-rot (rotation-y half-pi)
          p (tup/point 0 0 1)]
      (is (tup/tup-eq? (tup/point rad2-over-2 0 rad2-over-2) (mat/mat-mul-tup eight-rot p)))
      (is (tup/tup-eq? (tup/point 1 0 0) (mat/mat-mul-tup quarter-rot p)))))

  (testing "A point can be rotated about the z axis"
    (let [eight-rot (rotation-z quarter-pi)
          quarter-rot (rotation-z half-pi)
          p (tup/point 0 1 0)]
      (is (tup/tup-eq? (tup/point (- rad2-over-2) rad2-over-2 0) (mat/mat-mul-tup eight-rot p)))
      (is (tup/tup-eq? (tup/point -1 0 0) (mat/mat-mul-tup quarter-rot p))))))

(deftest shearing-test
  (testing "Shearing transfomation can move x in proportion to y"
    (let [transform (shearing 1 0 0 0 0 0)
          p (tup/point 2 3 4)]
      (is (tup/tup-eq? (tup/point 5 3 4) (mat/mat-mul-tup transform p)))))

  (testing "Shearing transfomation can move x in proportion to z"
    (let [transform (shearing 0 1 0 0 0 0)
          p (tup/point 2 3 4)]
      (is (tup/tup-eq? (tup/point 6 3 4) (mat/mat-mul-tup transform p)))))

  (testing "Shearing transfomation can move y in proportion to x"
    (let [transform (shearing 0 0 1 0 0 0)
          p (tup/point 2 3 4)]
      (is (tup/tup-eq? (tup/point 2 5 4) (mat/mat-mul-tup transform p)))))

  (testing "Shearing transfomation can move y in proportion to z"
    (let [transform (shearing 0 0 0 1 0 0)
          p (tup/point 2 3 4)]
      (is (tup/tup-eq? (tup/point 2 7 4) (mat/mat-mul-tup transform p)))))

  (testing "Shearing transfomation can move z in proportion to x"
    (let [transform (shearing 0 0 0 0 1 0)
          p (tup/point 2 3 4)]
      (is (tup/tup-eq? (tup/point 2 3 6) (mat/mat-mul-tup transform p)))))

  (testing "Shearing transfomation can move z in proportion to y"
    (let [transform (shearing 0 0 0 0 0 1)
          p (tup/point 2 3 4)]
      (is (tup/tup-eq? (tup/point 2 3 7) (mat/mat-mul-tup transform p))))))

(deftest compoound-transfomations-test
  (let [p (tup/point 1 0 1)
        rot (rotation-x (/ math/PI 2))
        scale (scaling 5 5 5)
        trans (translation 10 5 7)]
    (testing "Transfomations can be applied individually"
      (let [p2 (mat/mat-mul-tup rot p)
            p3 (mat/mat-mul-tup scale p2)
            p4 (mat/mat-mul-tup trans p3)]
        (is (tup/tup-eq? (tup/point 1 -1 0) p2))
        (is (tup/tup-eq? (tup/point 5 -5 0) p3))
        (is (tup/tup-eq? (tup/point 15 0 7) p4))))

    (testing "Chained transformations must be applied in reverse order"
      (let [transform (mat/mat-mul trans scale rot)]
        (is (tup/tup-eq? (tup/point 15 0 7) (mat/mat-mul-tup transform p)))))))

(deftest view-transformation-test
  (testing "The view transform for default orientation is identity matrix"
    (let [from (tup/point 0 0 0)
          to (tup/point 0 0 -1)
          up (tup/vect 0 1 0)]
      (is (mat/mat-eq? mat/identity-matrix (view-transform from to up)))))

  (testing "The view transfom for looking in positive z direction is rotated"
    (let [from (tup/point 0 0 0)
          to (tup/point 0 0 1)
          up (tup/vect 0 1 0)]
      (is (mat/mat-eq? (scaling -1 1 -1) (view-transform from to up)))))

  (testing "The view transform moved the workd, not the eye"
    (let [from (tup/point 0 0 8)
          to (tup/point 0 0 0)
          up (tup/vect 0 1 0)]
      (is (mat/mat-eq? (translation 0 0 -8) (view-transform from to up)))))

  (testing "An arbitrary view transformation"
    (let [from (tup/point 1 3 2)
          to (tup/point 4 -2 8)
          up (tup/vect 1 1 0)
          expected (mat/mat4x4
                    -0.50709 0.50709 0.67612 -2.36643
                    0.76772 0.60609 0.12122 -2.82843
                    -0.35857 0.59761 -0.71714 0.00000
                    0.00000 0.00000 0.00000 1.00000)]
      (is (mat/mat-eq? expected (view-transform from to up))))))
