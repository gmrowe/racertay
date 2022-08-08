(ns racertay.pattern-test
  (:require [clojure.test :refer :all]
            [racertay.pattern :refer :all]
            [racertay.color :as color]
            [racertay.tuple :as tup]))

(deftest stripe-pattern-creation-test
  (testing "A stripe pattern is created with two colors"
    (let [patt (stripe-pattern color/white color/black)]
      (is (color/color-eq? color/white (:stripe-pattern/a patt)))
      (is (color/color-eq? color/black (:stripe-pattern/b patt))))))

(deftest stripe-at-test
  (testing "A stripe pattern is constant in y"
    (let [patt (stripe-pattern color/white color/black)]
      (is (color/color-eq? color/white (stripe-at patt (tup/point 0 0 0))))
      (is (color/color-eq? color/white (stripe-at patt (tup/point 0 1 0))))
      (is (color/color-eq? color/white (stripe-at patt (tup/point 0 2 0))))))

  (testing "A stripe pattern is constant in z"
    (let [patt (stripe-pattern color/white color/black)]
      (is (color/color-eq? color/white (stripe-at patt (tup/point 0 0 0))))
      (is (color/color-eq? color/white (stripe-at patt (tup/point 0 0 1))))
      (is (color/color-eq? color/white (stripe-at patt (tup/point 0 0 2))))))

  (testing "A stripe pattern alternates in x"
    (let [patt (stripe-pattern color/white color/black)]
      (is (color/color-eq? color/white (stripe-at patt (tup/point 0 0 0))))
      (is (color/color-eq? color/white (stripe-at patt (tup/point 0.9 0 0))))
      (is (color/color-eq? color/black (stripe-at patt (tup/point 1 0 0))))
      (is (color/color-eq? color/black (stripe-at patt (tup/point -0.1 0 0))))
      (is (color/color-eq? color/black (stripe-at patt (tup/point -1 0 0))))
      (is (color/color-eq? color/white (stripe-at patt (tup/point -1.1 0 0)))))))
