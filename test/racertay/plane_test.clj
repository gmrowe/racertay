(ns racertay.plane-test
  (:require [clojure.test :refer :all]
            [racertay.plane :refer :all]
            [racertay.ray :as ray]
            [racertay.tuple :as tup]
            [racertay.fcmp :as fcmp]
            [racertay.protocols :as p]))

(deftest local-normal-test
  (testing "The normal of a plane is contant everywhere"
    (let [p (plane)
          n1 (p/local-normal-at p (tup/point 0 0 0))
          n2 (p/local-normal-at p (tup/point 10 0 -10))
          n3 (p/local-normal-at p (tup/point -5 0 150))]
      (is (tup/tup-eq? n1 n2 n1)))))

(deftest local-intersect-test
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
