(ns racertay.material-test
  (:require [clojure.test :refer :all]
            [racertay.material :refer :all]
            [racertay.color :as color]
            [racertay.fcmp :as fcmp]
            [racertay.tuple :as tup]
            [racertay.light :as light]))

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

(deftest lighting-test
  (let [m (material)
        surface-pos (tup/point 0 0 0)]
    (testing "Lighting with the eye between the light and the surface"
      (let [eyev (tup/vect 0 0 -1)
            normalv (tup/vect 0 0 -1)
            light (light/point-light (tup/point 0 0 -10) (color/color 1 1 1))]
        (is (color/color-eq?
             (color/color 1.9 1.9 1.9)
             (lighting m light surface-pos eyev normalv)))))

    (testing "Lighting with eye betweem the light and surface - eye offset 45 deg"
      (let [rad-2-over-2 (/ (Math/sqrt 2) 2)
            eyev (tup/vect 0 rad-2-over-2 (- rad-2-over-2))
            normalv (tup/vect 0 0 -1)
            light (light/point-light (tup/point 0 0 -10) (color/color 1 1 1))]
        (is (color/color-eq?
             (color/color 1 1 1)
             (lighting m light surface-pos eyev normalv)))))

    (testing "Lighting with eye opposite surface - light offset 45 deg"
      (let [eyev (tup/vect 0 0 -1)
            normalv (tup/vect 0 0 -1)
            light (light/point-light (tup/point 0 10 -10) (color/color 1 1 1))]
        (is (color/color-eq?
             (color/color 0.7364 0.7364 0.7364)
             (lighting m light surface-pos eyev normalv)))))

    (testing "Lighting with eye in path of reflection vector"
      (let [rad-2-over-2 (/ (Math/sqrt 2) 2)
            eyev (tup/vect 0 (- rad-2-over-2) (- rad-2-over-2))
            normalv (tup/vect 0 0 -1)
            light (light/point-light (tup/point 0 10 -10) (color/color 1 1 1))]
        (is (color/color-eq?
             (color/color 1.6364 1.6364 1.6364)
             (lighting m light surface-pos eyev normalv)))))

    (testing "Lighting with light behind surface"
      (let [eyev (tup/vect 0 0 -1)
            normalv (tup/vect 0 0 -1)
            light (light/point-light (tup/point 0 0 10) (color/color 1 1 1))]
        (is (color/color-eq?
             (color/color 0.1 0.1 0.1)
             (lighting m light surface-pos eyev normalv)))))))
