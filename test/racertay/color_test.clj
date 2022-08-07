(ns racertay.color-test
  (:require [clojure.test :refer :all]
            [racertay.fcmp :refer :all]
            [racertay.color :refer :all]))

(deftest create-color-test
  (testing "a color can be created from red green and blue values"
    (let [c (color -0.5 0.4 1.7)]
      (is (nearly-eq? -0.5 (:red c)))
      (is (nearly-eq? 0.4 (:green c)))
      (is (nearly-eq? 1.7 (:blue c))))))

(deftest hex->color-test
  (testing "a zero hex-value converts to black"
    (let [c (hex->color 0x000000)]
      (is (color-eq? black c))))

  (testing "the first two nibbles in a hex value are the red components"
    (let [c (hex->color 0xff0000)]
      (is (color-eq? red c))))

  (testing "the second two nibbles in a hex value are the green components"
    (let [c (hex->color 0x00ff00)]
      (is (color-eq? lime c))))

  (testing "the last two nibbles in a hex value are the blue components"
    (let [c (hex->color 0x0000ff)]
      (is (color-eq? blue c))))

  (testing "an arbitrary hex can be converted to a color"
    (let [c (hex->color 0xD1AE32)]
      (is (color-eq? (color 0.81961 0.68235 0.19608) c)))))
(deftest color-equality-test
  (testing "two colors are equal if their r/g/b components are equal"
    (is (color-eq? (color 1 2 3) (color 1.0 2.0 3.0)))))

(deftest adding-colors-test
  (testing "two colors can be added"
    (let [c1 (color 0.9 0.6 0.75)
          c2 (color 0.7 0.1 0.25)]
      (is (color-eq? (color 1.6 0.7 1.0) (color-add c1 c2)))))

  (testing "a variable number of colors can be added"
    (let [c1 (color 0.9 0.6 0.75)
          c2 (color 0.7 0.1 0.25)
          c3 (color 0.5 0.2 0.23)]
      (is (color-eq? (color 2.1 0.9 1.23) (color-add c1 c2 c3)))))

  (testing "a single color can be added"
    (let [c (color 1 2 3)]
      (is (color-eq? c (color-add c))))))

(deftest subtracting-colors-test
  (testing "two colors can be subtracted"
    (let [c1 (color 0.9 0.6 0.75)
          c2 (color 0.7 0.1 0.25)]
      (is (color-eq? (color 0.2 0.5 0.5) (color-sub c1 c2)))))

  (testing "a variable number of colors can be subtracted"
    (let [c1 (color 0.9 0.6 0.75)
          c2 (color 0.7 0.1 0.25)
          c3 (color 0.5 0.2 0.23)]
      (is (color-eq? (color -0.3 0.3 0.27) (color-sub c1 c2 c3)))))

  (testing "a single color can be subtracted"
    (let [c (color 1 2 -3)]
      (is (color-eq? (color -1 -2 3) (color-sub c))))))

(deftest color-scalar-multiplication-test
  (testing "a color can be multiplied by a scalar"
    (let [c (color 0.2 0.3 0.4)]
      (is (color-eq? (color 0.4 0.6 0.8) (color-mul-scalar c 2))))))

(deftest color-mul-test
  (testing "a color can be multiplied by another color"
    (let [c1 (color 1 0.2 0.4)
          c2 (color 0.9 1 0.1)]
      (is (color-eq? (color 0.9 0.2 0.04) (color-mul c1 c2))))))
