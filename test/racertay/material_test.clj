(ns racertay.material-test
  (:require [clojure.test :refer :all]
            [racertay.material :refer :all]
            [racertay.color :as color]
            [racertay.fcmp :as fcmp]
            [racertay.tuple :as tup]
            [racertay.light :as light]))

(deftest material-creation-test
  (testing "Material has a default color"
    (is (color/color-eq? (color/color 1 1 1) (:material/color new-material))))

  (testing "Material has default ambient attribute"
    (is (fcmp/nearly-eq? 0.1 (:material/ambient new-material))))

  (testing "Material has default diffuse attribute"
    (is (fcmp/nearly-eq? 0.9 (:material/diffuse new-material))))

  (testing "Material has default specular attribute"
    (is (fcmp/nearly-eq? 0.9 (:material/specular new-material))))

  (testing "Material has default shininess attribute"
    (is (fcmp/nearly-eq? 200.0 (:material/shininess new-material)))))

(deftest material-assoc-test
  (testing "Material color can be assoc'ed"
    (let [new-color (color/color 0 1 1)
          material (assoc new-material :material/color new-color)]
      (is (color/color-eq? new-color (:material/color material)))))

  (testing "Material ambient attribute can be assoc'ed"
    (let [new-ambient 0.5
          material (assoc new-material :material/ambient new-ambient)]
      (is (fcmp/nearly-eq? new-ambient (:material/ambient material)))))

  (testing "Material diffuse attribute can be assoc'ed"
    (let [new-diffuse 0.5
          material (assoc new-material :material/diffuse new-diffuse)]
      (is (fcmp/nearly-eq? new-diffuse (:material/diffuse material)))))

  (testing "Material specular attribute can be assoc'ed"
    (let [new-specular 0.5
          material (assoc  new-material :material/specular new-specular)]
      (is (fcmp/nearly-eq? new-specular (:material/specular material)))))

  (testing "Material shininess attribute can be assoc'ed"
    (let [new-shininess 300.0
          material (assoc  new-material :material/shininess new-shininess)]
      (is (fcmp/nearly-eq? new-shininess (:material/shininess material))))))

(deftest material-eq?-test
  (testing "material-eq? returns true for two default materials"
    (is (material-eq? new-material new-material)))

  (testing "material-eq? returns false when material is not equal"
    (is (not
         (material-eq?
          new-material (assoc new-material :material/shininess 300.0))))))

(deftest lighting-test
  (let [m new-material
        surface-pos (tup/point 0 0 0)]
    (testing "Lighting with the eye between the light and the surface"
      (let [eyev (tup/vect 0 0 -1)
            normalv (tup/vect 0 0 -1)
            light (light/point-light (tup/point 0 0 -10) (color/color 1 1 1))
            in-shadow false]
        (is (color/color-eq?
             (color/color 1.9 1.9 1.9)
             (lighting m light surface-pos eyev normalv in-shadow)))))

    (testing "Lighting with eye betweem the light and surface - eye offset 45 deg"
      (let [rad-2-over-2 (/ (Math/sqrt 2) 2)
            eyev (tup/vect 0 rad-2-over-2 (- rad-2-over-2))
            normalv (tup/vect 0 0 -1)
            light (light/point-light (tup/point 0 0 -10) (color/color 1 1 1))
            in-shadow false]
        (is (color/color-eq?
             (color/color 1 1 1)
             (lighting m light surface-pos eyev normalv in-shadow)))))

    (testing "Lighting with eye opposite surface - light offset 45 deg"
      (let [eyev (tup/vect 0 0 -1)
            normalv (tup/vect 0 0 -1)
            light (light/point-light (tup/point 0 10 -10) (color/color 1 1 1))
            in-shadow false]
        (is (color/color-eq?
             (color/color 0.7364 0.7364 0.7364)
             (lighting m light surface-pos eyev normalv in-shadow)))))

    (testing "Lighting with eye in path of reflection vector"
      (let [rad-2-over-2 (/ (Math/sqrt 2) 2)
            eyev (tup/vect 0 (- rad-2-over-2) (- rad-2-over-2))
            normalv (tup/vect 0 0 -1)
            light (light/point-light (tup/point 0 10 -10) (color/color 1 1 1))
            in-shadow false]
        (is (color/color-eq?
             (color/color 1.6364 1.6364 1.6364)
             (lighting m light surface-pos eyev normalv in-shadow)))))

    (testing "Lighting with light behind surface"
      (let [eyev (tup/vect 0 0 -1)
            normalv (tup/vect 0 0 -1)
            light (light/point-light (tup/point 0 0 10) (color/color 1 1 1))
            in-shadow false]
        (is (color/color-eq?
             (color/color 0.1 0.1 0.1)
             (lighting m light surface-pos eyev normalv in-shadow)))))

    (testing "Lighting when a surface is in shadow"
      (let [eyev (tup/vect 0 0 -1)
            normalv (tup/vect 0 0 -1)
            light (light/point-light (tup/point 0 0 -10) (color/color 1 1 1))
            in-shadow true]
        (is (color/color-eq? (color/color 0.1 0.1 0.1)
                             (lighting m light surface-pos eyev normalv in-shadow)))))))
