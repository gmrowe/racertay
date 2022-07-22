(ns racertay.ray-test
  (:require [clojure.test :refer :all]
            [racertay.ray :refer :all]
            [racertay.tuple :as tup]))

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
