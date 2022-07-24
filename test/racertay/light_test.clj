(ns racertay.light-test
  (:require [clojure.test :refer :all]
            [racertay.light :refer :all]
            [racertay.color :as color]
            [racertay.tuple :as tup]))

(deftest point-light-test
    (testing "A point light has a position and intensity"
      (let [p (tup/point 0 0 0)            
            i (color/color 1 1 1)
            light (point-light p i)]
        (is (color/color-eq? i (intensity light)))
        (is (tup/tup-eq? p (position light))))))
