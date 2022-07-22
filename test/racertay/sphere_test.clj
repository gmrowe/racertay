(ns racertay.sphere-test
  (:require [clojure.test :refer :all]
            [racertay.ray :as ray]
            [racertay.tuple :as tup]
            [racertay.fcmp :as fcmp]
            [racertay.sphere :refer :all]
            [racertay.intersection :as inter]))

(deftest sphere-creation-test
  (testing "A sphere must always return a unique value"
    (let [s1 (sphere)
          s2 (sphere)]
      (is (= s1 s1))
      (is (not= s1 s2)))))
  
(deftest sphere-ray-intersection-test
  (testing "A ray which intersects a sphere at two points"
    (let [r (ray/ray (tup/point 0 0 -5) (tup/vect 0 0 1))
          s (sphere)
          xs (intersect s r)]
      (is (= 2 (count xs)))
      (is (fcmp/nearly-eq? 4.0 (inter/t (nth xs 0))))
      (is (fcmp/nearly-eq? 6.0 (inter/t (nth xs 1))))))

  (testing "A ray which intersects a sphere at a tangent"
    (let [r (ray/ray (tup/point 0 1 -5) (tup/vect 0 0 1))
          s (sphere)
          xs (intersect s r)]
      (is (= 2 (count xs)))
      (is (fcmp/nearly-eq? 5.0 (inter/t (nth xs 0))))
      (is (fcmp/nearly-eq? 5.0 (inter/t (nth xs 1))))))

  (testing "A ray which misses a sphere"
    (let [r (ray/ray (tup/point 0 2 -5) (tup/vect 0 0 1))
          s (sphere)
          xs (intersect s r)]
      (is (zero? (count xs)))))

  (testing "A ray which originates inside a sphere"
    (let [r (ray/ray (tup/point 0 0 0) (tup/vect 0 0 1))
          s (sphere)
          xs (intersect s r)]
      (is (= 2 (count xs)))
      (is (fcmp/nearly-eq? -1.0 (inter/t (nth xs 0))))
      (is (fcmp/nearly-eq? 1.0 (inter/t (nth xs 1))))))

  (testing "A sphere that is completly behind a ray"
    (let [r (ray/ray (tup/point 0 0 5) (tup/vect 0 0 1))
          s (sphere)
          xs (intersect s r)]
      (is (= 2 (count xs)))
      (is (fcmp/nearly-eq? -6.0 (inter/t (nth xs 0))))
      (is (fcmp/nearly-eq? -4.0 (inter/t (nth xs 1))))))

  (testing "intersect sets the object on the intersecton"
    (let [r (ray/ray (tup/point 0 0 -5) (tup/vect 0 0 1))
          s (sphere)
          xs (intersect s r)]
      (is (= 2 (count xs)))
      (is (= s (inter/object (nth xs 0))))
      (is (= s (inter/object (nth xs 1)))))))
