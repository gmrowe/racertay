(ns racertay.intersection-test
  (:require [clojure.test :refer :all]
            [racertay.intersection :refer :all]
            [racertay.sphere :as sphere]
            [racertay.fcmp :as fcmp]
            [racertay.ray :as ray]
            [racertay.tuple :as tup]
            [racertay.transformations :as xform]
            [racertay.protocols :as p]))

(deftest intersection-creation-test
  (testing "An intersection encapsulates a t an an object"
    (let [s (sphere/sphere)
          i (intersection 3.5 s)]
      (is (fcmp/nearly-eq? 3.5 (:intersection/t i)))
      (is (= s (:intersection/object i))))))

(deftest intersection-aggregation-test
  (testing "Intersections should be able to be aggregated"
    (let [s (sphere/sphere)
          i1 (intersection 1 s)
          i2 (intersection 2 s)
          xs (intersections i1 i2)]
      (is (= 2 (count xs)))
      (is (fcmp/nearly-eq? 1 (:intersection/t (nth xs 0))))
      (is (fcmp/nearly-eq? 2 (:intersection/t (nth xs 1)))))))

(deftest intersection-hit-test
  (testing "The hit when all intersections have positive t"
    (let [s (sphere/sphere)
          i1 (intersection 1 s)
          i2 (intersection 2 s)
          xs (intersections i2 i1)
          i (hit xs)]
      (is (= i1 i))))

  (testing "The hit when some intersections have negative t"
    (let [s (sphere/sphere)
          i1 (intersection -1 s)
          i2 (intersection 1 s)
          xs (intersections i2 i1)
          i (hit xs)]
      (is (= i2 i))))

  (testing "The hit when all intersectins have negative t"
    (let [s (sphere/sphere)
          i1 (intersection -1 s)
          i2 (intersection -2 s)
          xs (intersections i2 i1)
          i (hit xs)]
      (is (nil? i))))

  (testing "The hit is always the lowest nonnegative intersection"
    (let [s (sphere/sphere)
          i1 (intersection 5 s)
          i2 (intersection 7 s)
          i3 (intersection -3 s)
          i4 (intersection 2 s)
          xs (intersections i1 i2 i3 i4)
          i (hit xs)]
      (is (= i4 i)))))

(deftest prepare-computations-test
  (testing "The state of an intersection is precomputed"
    (let [r (ray/ray (tup/point 0 0 -5) (tup/vect 0 0 1))
          shape (sphere/sphere)
          i (intersection 4 shape)
          comps (prepare-computations i r)]
      (is (fcmp/nearly-eq? (:intersection/t i) (:intersection/t comps)))
      (is (= (:intersection/object i) (:intersection/object comps)))
      (is (tup/tup-eq? (tup/point 0 0 -1) (:intersection/point comps)))
      (is (tup/tup-eq? (tup/vect 0 0 -1) (:intersection/normalv comps)))
      (is (tup/tup-eq? (tup/vect 0 0 -1) (:intersection/eyev comps)))))

  (testing "The :inside attribute is false when intersection occurs outside"
    (let [r (ray/ray (tup/point 0 0 -5) (tup/vect 0 0 1))
          shape (sphere/sphere)
          i (intersection 4 shape)
          comps (prepare-computations i r)]
      (is (false? (:intersection/inside comps)))))

  (testing "The :inside attribute is true when intersection occurs inside"
    (let [r (ray/ray (tup/point 0 0 0) (tup/vect 0 0 1))
          shape (sphere/sphere)
          i (intersection 1 shape)
          comps (prepare-computations i r)]
      (is (tup/tup-eq? (tup/point 0 0 1) (:intersection/point comps)))
      (is (tup/tup-eq? (tup/vect 0 0 -1) (:intersection/eyev comps)))
      (is (true? (:intersection/inside comps)))
      (is (tup/tup-eq? (tup/vect 0 0 -1) (:intersection/normalv comps)))))

  (testing "The :over-point attribute should offset the hit"
    (let [r (ray/ray (tup/point 0 0 -5) (tup/vect 0 0 1))
          shape (-> (sphere/sphere)
                    (p/apply-transform (xform/translation 0 0 1)))
          i (intersection 5 shape)
          comps (prepare-computations i r)]
      (is (< (tup/z (:intersection/over-point comps)) (/ fcmp/epsilon 2)))
      (is (< (tup/z (:intersection/over-point comps))
             (tup/z (:intersection/point comps))))))

  (testing "When the intersection occurs inside :over-point shold be inside"
    (let [r (ray/ray (tup/point 0 0 0) (tup/vect 0 0 1))
          shape (sphere/sphere)
          i (intersection 1 shape)
          comps (prepare-computations i r)
          diff-z (- (tup/z (:intersection/over-point comps))
                    (tup/z (:intersection/point comps)))]
      (is (< diff-z (- (/ fcmp/epsilon 2))))
      (is (< (tup/z (:intersection/over-point comps))
             (tup/z (:intersection/point comps)))))))
