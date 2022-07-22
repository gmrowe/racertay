(ns racertay.intersection-test
  (:require [clojure.test :refer :all]
            [racertay.sphere :as sphere]
            [racertay.fcmp :as fcmp]
            [racertay.intersection :refer :all]))

(deftest intersection-creation-test
  (testing "An intersection encapsulates a t an an object"
    (let [s (sphere/sphere)
          i (intersection 3.5 s)]
      (is (fcmp/nearly-eq? 3.5 (t i)))
      (is (= s (object i))))))

(deftest intersection-aggregation-test
  (testing "Intersections should be able to be aggregated"
    (let [s (sphere/sphere)
          i1 (intersection 1 s)
          i2 (intersection 2 s)
          xs (intersections i1 i2)]
      (is (= 2 (count xs)))
      (is (fcmp/nearly-eq? 1 (t (nth xs 0))))
      (is (fcmp/nearly-eq? 2 (t (nth xs 1)))))))

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
