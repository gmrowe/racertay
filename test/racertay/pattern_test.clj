(ns racertay.pattern-test
  (:require [clojure.test :refer :all]
            [racertay.pattern :refer :all]
            [racertay.color :as color]
            [racertay.tuple :as tup]
            [racertay.sphere :as sphere]
            [racertay.protocols :as p]
            [racertay.transformations :as xform]
            [racertay.matrix :as matrix]))

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

(deftest stripe-at-object-test
  (testing "Stripes with an object transformation"
    (let [object (-> (sphere/sphere)
                     (p/apply-transform (xform/scaling 2 2 2)))
          pattern (stripe-pattern color/white color/black)
          c (stripe-at-object pattern object (tup/point 1.5 0 0))]
      (is (color/color-eq? color/white c))))

  (testing "Stripes with a pattern transformation"
    (let [object (sphere/sphere)
          pattern (-> (stripe-pattern color/white color/black)
                      (apply-transform (xform/scaling 2 2 2)))
          c (stripe-at-object pattern object (tup/point 1.5 0 0))]
      (is (color/color-eq? color/white c))))

  (testing "Stripes with both object and pattern transformations"
    (let [object (-> (sphere/sphere)
                     (p/apply-transform (xform/scaling 2 2 2)))
          pattern (-> (stripe-pattern color/white color/black)
                      (apply-transform (xform/translation 0.5 0 0)))
          c (stripe-at-object pattern object (tup/point 2.5 0 0))]
      (is (color/color-eq? color/white c)))))
