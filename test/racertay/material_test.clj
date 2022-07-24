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

(deftest material-assoc-test
  (testing "Material color can be assoc'ed"
    (let [new-color (color/color 0 1 1)
          material (assoc-color (material) new-color)]
      (is (color/color-eq? new-color (color material)))))

  (testing "Material ambient attribute can be assoc'ed"
    (let [new-ambient 0.5
          material (assoc-ambient (material) new-ambient)]
      (is (fcmp/nearly-eq? new-ambient (ambient material)))))

  (testing "Material diffuse attribute can be assoc'ed"
    (let [new-diffuse 0.5
          material (assoc-diffuse (material) new-diffuse)]
      (is (fcmp/nearly-eq? new-diffuse (diffuse material)))))

  (testing "Material specular attribute can be assoc'ed"
    (let [new-specular 0.5
          material (assoc-specular (material) new-specular)]
      (is (fcmp/nearly-eq? new-specular (specular material)))))

  (testing "Material shininess attribute can be assoc'ed"
    (let [new-shininess 300.0
          material (assoc-shininess (material) new-shininess)]
      (is (fcmp/nearly-eq? new-shininess (shininess material))))))

(deftest material-eq?-test
  (testing "material-eq? returns true for two default materials"
    (is (material-eq? (material) (material))))

  (testing "material-eq? returns false when material is not equal"
    (is (not (material-eq? (material) (assoc-shininess (material) 300.0))))))
