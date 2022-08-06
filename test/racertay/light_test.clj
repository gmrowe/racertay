(ns racertay.light-test
  (:require [clojure.test :refer :all]
            [racertay.light :refer :all]
            [racertay.color :as color]
            [racertay.tuple :as tup]))

(deftest point-light-creation-test
  (testing "A point light has a position and intensity"
    (let [p (tup/point 0 0 0)            
          i (color/color 1 1 1)
          light (point-light p i)]
      (is (color/color-eq? i (:light/intensity light)))
      (is (tup/tup-eq? p (:light/position light))))))

(deftest light-eq?-test
  (testing "Lights with different positions are not equal"
    (let [color (color/color 1 1 1)]
      (is (not (eq? (point-light (tup/point 0 0 0) color)
                    (point-light (tup/point 0 0 1) color))))))

  (testing "Lights with different colors are not equal"
    (let [position (tup/point 0 0 0)]
      (is (not (eq? (point-light position (color/color 0 0 0))
                    (point-light position (color/color 1 0 0)))))))

  (testing "Lights with same position and color are equal"
    (let [color (color/color 1 1 1)
          position (tup/point 0 0 0)]
      (is (eq? (point-light position color) (point-light position color))))))

