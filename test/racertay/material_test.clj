(ns racertay.material-test
  (:require [clojure.test :refer :all]
            [racertay.material :refer :all]
            [racertay.color :as color]
            [racertay.fcmp :as fcmp]))

(deftest material-creation-test
  (testing "Material has a default color"
    (is (color/color-eq? (color/color 1 1 1) (color (material)))))

  (testing "Material has default ambient attribute"
    (is (fcmp/nearly-eq? 0.1 (ambient (material)))))

  (testing "Material has default diffuse attribute"
    (is (fcmp/nearly-eq? 0.9 (diffuse (material)))))

  (testing "Material has default specular attribute"
    (is (fcmp/nearly-eq? 0.9 (specular (material)))))

  (testing "Material has default shininess attribute"
    (is (fcmp/nearly-eq? 200.0 (shininess (material))))))
