(ns racertay.material-test
  (:require [clojure.test :refer :all]
            [racertay.material :refer :all]
            [racertay.color :as color]
            [racertay.fcmp :as fcmp]
            [racertay.tuple :as tup]
            [racertay.light :as light]
            [racertay.pattern :as patt]
            [racertay.shape :as shape]))

(deftest material-creation-test
  (testing "Material has a default color"
    (is (color/color-eq? color/white (:material/color default-material))))

  (testing "Material has default ambient attribute"
    (is (fcmp/nearly-eq? 0.1 (:material/ambient default-material))))

  (testing "Material has default diffuse attribute"
    (is (fcmp/nearly-eq? 0.9 (:material/diffuse default-material))))

  (testing "Material has default specular attribute"
    (is (fcmp/nearly-eq? 0.9 (:material/specular default-material))))

  (testing "Material has default shininess attribute"
    (is (fcmp/nearly-eq? 200.0 (:material/shininess default-material))))

  (testing "Material has a default reflectivity"
    (is (fcmp/nearly-zero? (:material/reflective default-material)))))

(deftest material-assoc-test
  (testing "Material color can be assoc'ed"
    (let [new-color (color/color 0 1 1)
          material (assoc default-material :material/color new-color)]
      (is (color/color-eq? new-color (:material/color material)))))

  (testing "Material ambient attribute can be assoc'ed"
    (let [new-ambient 0.5
          material (assoc default-material :material/ambient new-ambient)]
      (is (fcmp/nearly-eq? new-ambient (:material/ambient material)))))

  (testing "Material diffuse attribute can be assoc'ed"
    (let [new-diffuse 0.5
          material (assoc default-material :material/diffuse new-diffuse)]
      (is (fcmp/nearly-eq? new-diffuse (:material/diffuse material)))))

  (testing "Material specular attribute can be assoc'ed"
    (let [new-specular 0.5
          material (assoc  default-material :material/specular new-specular)]
      (is (fcmp/nearly-eq? new-specular (:material/specular material)))))

  (testing "Material shininess attribute can be assoc'ed"
    (let [new-shininess 300.0
          material (assoc  default-material :material/shininess new-shininess)]
      (is (fcmp/nearly-eq? new-shininess (:material/shininess material))))))

(deftest material-eq?-test
  (testing "material-eq? returns true for two default materials"
    (is (material-eq? default-material default-material)))

  (testing "material-eq? returns false when material is not equal"
    (is (not
         (material-eq?
          default-material (assoc default-material :material/shininess 300.0))))))

(deftest lighting-test
  (let [m default-material
        surface-pos (tup/point 0 0 0)
        object (shape/sphere)]
    (testing "Lighting with the eye between the light and the surface"
      (let [eyev (tup/vect 0 0 -1)
            normalv (tup/vect 0 0 -1)
            light (light/point-light (tup/point 0 0 -10) color/white)
            in-shadow false]
        (is (color/color-eq?
             (color/color 1.9 1.9 1.9)
             (lighting m object light surface-pos eyev normalv in-shadow)))))

    (testing "Lighting with eye betweem the light and surface - eye offset 45 deg"
      (let [rad-2-over-2 (/ (Math/sqrt 2) 2)
            eyev (tup/vect 0 rad-2-over-2 (- rad-2-over-2))
            normalv (tup/vect 0 0 -1)
            light (light/point-light (tup/point 0 0 -10) color/white)
            in-shadow false]
        (is (color/color-eq?
             color/white
             (lighting m object light surface-pos eyev normalv in-shadow)))))

    (testing "Lighting with eye opposite surface - light offset 45 deg"
      (let [eyev (tup/vect 0 0 -1)
            normalv (tup/vect 0 0 -1)
            light (light/point-light (tup/point 0 10 -10) color/white)
            in-shadow false]
        (is (color/color-eq?
             (color/color 0.7364 0.7364 0.7364)
             (lighting m object light surface-pos eyev normalv in-shadow)))))

    (testing "Lighting with eye in path of reflection vector"
      (let [rad-2-over-2 (/ (Math/sqrt 2) 2)
            eyev (tup/vect 0 (- rad-2-over-2) (- rad-2-over-2))
            normalv (tup/vect 0 0 -1)
            light (light/point-light (tup/point 0 10 -10) color/white)
            in-shadow false]
        (is (color/color-eq?
             (color/color 1.6364 1.6364 1.6364)
             (lighting m object light surface-pos eyev normalv in-shadow)))))

    (testing "Lighting with light behind surface"
      (let [eyev (tup/vect 0 0 -1)
            normalv (tup/vect 0 0 -1)
            light (light/point-light (tup/point 0 0 10) color/white)
            in-shadow false]
        (is (color/color-eq?
             (color/color 0.1 0.1 0.1)
             (lighting m object light surface-pos eyev normalv in-shadow)))))

    (testing "Lighting when a surface is in shadow"
      (let [eyev (tup/vect 0 0 -1)
            normalv (tup/vect 0 0 -1)
            light (light/point-light (tup/point 0 0 -10) color/white)
            in-shadow true]
        (is (color/color-eq? (color/color 0.1 0.1 0.1)
                             (lighting m object light surface-pos eyev normalv in-shadow)))))

    (testing "Lighting with a pattern applied"
      (let [pattern (patt/stripe-pattern color/white color/black)
            material (-> default-material
                         (assoc :material/pattern pattern)
                         (assoc :material/ambient 1)
                         (assoc :material/diffuse 0)
                         (assoc :material/specular 0))
            eyev (tup/vect 0 0 -1)
            normalv (tup/vect 0 0 -1)
            light (light/point-light (tup/point 0 0 -10) color/white)
            c1 (lighting material object light (tup/point 0.9 0 0) eyev normalv false)
            c2 (lighting material object light (tup/point 1.1 0 0) eyev normalv false)]
        (is (color/color-eq? c1 color/white))
        (is (color/color-eq? c2 color/black))))))
