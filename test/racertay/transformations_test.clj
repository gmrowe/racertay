(ns racertay.transformations-test
  (:require [clojure.test :refer :all]
            [racertay.transformations :refer :all]
            [racertay.tuple :refer [point tup-eq? vect]]
            [racertay.matrix :refer [mat-mul mat-mul-tup inverse]]))

(deftest translation-test
  (testing "Multiplying a translation matrix by a point moves the point"
    (let [transform (translation 5 -3 2)
          p (point -3 4 5)]
      (is (tup-eq? (point 2 1 7) (mat-mul-tup transform p)))))

  (testing "Multiplying inverse of a translation matrix by a point moves point in reverse"
    (let [transform (translation 5 -3 2)
          p (point -3 4 5)]
      (is (tup-eq? (point -8 7 3) (mat-mul-tup (inverse transform) p)))))

  (testing "Multiplying a translation matix by a vector does not change the vector"
    (let [transform (translation 5 -3 2)
          v (vect -3 4 5)]
      (is (tup-eq? v (mat-mul-tup transform v))))))

(deftest scaling-test
  (testing "Multiplying a scaling matrix by a point scales the point"
    (let [transform (scaling 2 3 4)
          p (point -4 6 8)]
      (is (tup-eq? (point -8 18 32) (mat-mul-tup transform p)))))

  (testing "Multiplying scaling matrix by a vector changes its length"
    (let [transform (scaling 2 3 4)
          v (vect -4 6 8)]
      (is (tup-eq? (vect -8 18 32) (mat-mul-tup transform v)))))

  (testing "Multiplying inverse of a scaling matrix by a vector changes its length"
    (let [transform (inverse (scaling 2 3 4))
          v (vect -4 6 8)]
      (is (tup-eq? (vect -2 2 2) (mat-mul-tup transform v)))))

  (testing "Reflection is scaling by a negative value"
    (let [transform (scaling -1 1 1)
          p (point 2 3 4)]
      (is (tup-eq? (point -2 3 4) (mat-mul-tup transform p)))
      (is (tup-eq? (point -2 3 4) (mat-mul-tup x-reflection p))))))


(def rad2-over-2 (/ (Math/sqrt 2) 2))
(def quarter-pi (/ Math/PI 4))
(def half-pi (/ Math/PI 2))

(deftest rotation-test
  (testing "A point can be rotated about the x-axis"
    (let [eighth-rot (rotation-x quarter-pi)
          quarter-rot (rotation-x half-pi)
          p (point 0 1 0)]
      (is (tup-eq? (point 0 rad2-over-2 rad2-over-2) (mat-mul-tup eighth-rot p)))
      (is (tup-eq? (point 0 0 1) (mat-mul-tup quarter-rot p)))))

  (testing "The inverse of a rotation rotates in the opposite direction"
    (let [eighth-rot (rotation-x quarter-pi)
          quarter-rot (rotation-x half-pi)
          p (point 0 1 0)]
      (is (tup-eq? (point 0 rad2-over-2 (- rad2-over-2))
                   (mat-mul-tup (inverse eighth-rot) p)))
      (is (tup-eq? (point 0 0 -1) (mat-mul-tup (inverse quarter-rot) p)))))

  (testing "A point can be rotated about the y-axis"
    (let [eight-rot (rotation-y quarter-pi)
          quarter-rot (rotation-y half-pi)
          p (point 0 0 1)]
      (is (tup-eq? (point rad2-over-2 0 rad2-over-2) (mat-mul-tup eight-rot p)))
      (is (tup-eq? (point 1 0 0) (mat-mul-tup quarter-rot p)))))

  (testing "A point can be rotated about the z axis"
    (let [eight-rot (rotation-z quarter-pi)
          quarter-rot (rotation-z half-pi)
          p (point 0 1 0)]
      (is (tup-eq? (point (- rad2-over-2) rad2-over-2 0) (mat-mul-tup eight-rot p)))
      (is (tup-eq? (point -1 0 0) (mat-mul-tup quarter-rot p))))))

(deftest shearing-test
  (testing "Shearing transfomation can move x in proportion to y"
    (let [transform (shearing 1 0 0 0 0 0)
          p (point 2 3 4)]
      (is (tup-eq? (point 5 3 4) (mat-mul-tup transform p)))))

  (testing "Shearing transfomation can move x in proportion to z"
    (let [transform (shearing 0 1 0 0 0 0)
          p (point 2 3 4)]
      (is (tup-eq? (point 6 3 4) (mat-mul-tup transform p)))))

  (testing "Shearing transfomation can move y in proportion to x"
    (let [transform (shearing 0 0 1 0 0 0)
          p (point 2 3 4)]
      (is (tup-eq? (point 2 5 4) (mat-mul-tup transform p)))))

  (testing "Shearing transfomation can move y in proportion to z"
    (let [transform (shearing 0 0 0 1 0 0)
          p (point 2 3 4)]
      (is (tup-eq? (point 2 7 4) (mat-mul-tup transform p)))))

  (testing "Shearing transfomation can move z in proportion to x"
    (let [transform (shearing 0 0 0 0 1 0)
          p (point 2 3 4)]
      (is (tup-eq? (point 2 3 6) (mat-mul-tup transform p)))))

  (testing "Shearing transfomation can move z in proportion to y"
    (let [transform (shearing 0 0 0 0 0 1)
          p (point 2 3 4)]
      (is (tup-eq? (point 2 3 7) (mat-mul-tup transform p))))))

(deftest compoound-transfomations-test
  (let [p (point 1 0 1)
        rot (rotation-x (/ Math/PI 2))
        scale (scaling 5 5 5)
        trans (translation 10 5 7)]
    (testing "Transfomations can be applied individually"
      (let [p2 (mat-mul-tup rot p)
            p3 (mat-mul-tup scale p2)
            p4 (mat-mul-tup trans p3)]
        (is (tup-eq? (point 1 -1 0) p2))
        (is (tup-eq? (point 5 -5 0) p3))
        (is (tup-eq? (point 15 0 7) p4))))

    (testing "Chained transformations must be applied in reverse order"
      (let [transform (mat-mul trans scale rot)]
        (is (tup-eq? (point 15 0 7) (mat-mul-tup transform p)))))))
