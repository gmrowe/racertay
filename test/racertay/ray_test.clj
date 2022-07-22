(ns racertay.ray-test
  (:require [clojure.test :refer :all]
            [racertay.ray :refer :all]
            [racertay.tuple :as tup]
            [racertay.transformations :as xforms]))

(deftest ray-creation-test
  (testing "A ray can be created and queried"
    (let [ori (tup/point 1 2 3)
          dir (tup/vect 4 5 6)
          r (ray ori dir)]
      (is (tup/tup-eq? ori (origin r)))
      (is (tup/tup-eq? dir (direction r))))))

(deftest ray-casting-test
  (testing "A position can computed from a ray and distance (t)"
    (let [r (ray (tup/point 2 3 4) (tup/vect 1 0 0))]
      (is (tup/tup-eq? (tup/point 2 3 4) (position r 0)))
      (is (tup/tup-eq? (tup/point 3 3 4) (position r 1)))
      (is (tup/tup-eq? (tup/point 1 3 4) (position r -1)))
      (is (tup/tup-eq? (tup/point 4.5 3 4) (position r 2.5))))))

(deftest ray-transform-test
  (testing "A ray can be translated"
    (let [r (ray (tup/point 1 2 3) (tup/vect 0 1 0))
          m (xforms/translation 3 4 5)
          r2 (transform r m)]
      (is (tup/tup-eq? (tup/point 4 6 8) (origin r2)))
      (is (tup/tup-eq? (tup/vect 0 1 0) (direction r2)))))

  (testing "A ray can be scaled"
    (let [r (ray (tup/point 1 2 3) (tup/vect 0 1 0))
          m (xforms/scaling 2 3 4)
          r2 (transform r m)]
      (is (tup/tup-eq? (tup/point 2 6 12) (origin r2)))
      (is (tup/tup-eq? (tup/vect 0 3 0) (direction r2))))))
