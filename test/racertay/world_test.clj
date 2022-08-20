(ns racertay.world-test
  (:require [clojure.test :refer :all]
            [racertay.world :refer :all]
            [racertay.light :as light]
            [racertay.tuple :as tup]
            [racertay.color :as color]
            [racertay.shape :as shape]
            [racertay.material :as material]
            [racertay.transformations :as xform]
            [racertay.ray :as ray]
            [racertay.fcmp :as fcmp]
            [racertay.intersection :as intersection]
            [racertay.computations :as comps]
            [racertay.protocols :as p]
            [racertay.pattern-test :refer [test-pattern]]))

(def default-light (light/point-light (tup/point -10 10 -10) color/white))

(def sphere-1
  (let [material (-> material/default-material
                     (assoc :material/color (color/color 0.8 1.0 0.6))
                     (assoc :material/diffuse 0.7)
                     (assoc :material/specular 0.2))]
    (assoc (shape/sphere) :material material)))

(def sphere-2
  (let [scale (xform/scaling 0.5 0.5 0.5)]
    (shape/apply-transform (shape/sphere) scale)))

(def default-world
  (-> empty-world
      (assoc :world/light default-light)
      (update :world/objects conj sphere-1)
      (update :world/objects conj sphere-2)))

(deftest world-creation-test
  (testing "A new world has no objects and no light source"
    (let [w empty-world]
      (is (zero? (count (:world/objects w))))
      (is (nil? (:world/light w))))))

(deftest default-world-test
  (testing "The default world has default light"
    (is (light/eq? default-light (:world/light default-world))))

  (testing "The default world containd sphere-1"
    (is (some? (some #{sphere-1} (:world/objects default-world)))))

  (testing "The default world contains sphere-2"
    (is (some? (some #{sphere-2} (:world/objects default-world))))))

(deftest intersect-world-test
  (testing "A ray can intersect with a world"
    (let [ray (ray/ray (tup/point 0 0 -5) (tup/vect 0 0 1))
          xs (intersect-world default-world ray)]
      (is (= 4 (count xs)))
      (is (fcmp/nearly-eq? 4 (:intersection/t (nth xs 0))))
      (is (fcmp/nearly-eq? 4.5 (:intersection/t (nth xs 1))))
      (is (fcmp/nearly-eq? 5.5 (:intersection/t (nth xs 2))))
      (is (fcmp/nearly-eq? 6 (:intersection/t (nth xs 3)))))))

(deftest shade-hit-test
  (testing "An intersection can be shaded"
    (let [ray (ray/ray (tup/point 0 0 -5) (tup/vect 0 0 1))
          shape (nth (:world/objects default-world) 0)
          i (intersection/intersection 4 shape)
          comps (comps/prepare-computations i ray)
          c (shade-hit default-world comps)]
      (is (color/color-eq? (color/color 0.38066 0.47583 0.2855) c))))

  (testing "An intersection on the inside can be shaded"
    (let [light (light/point-light (tup/point 0 0.25 0) color/white)
          w (assoc default-world :world/light light)
          ray (ray/ray (tup/point 0 0 0) (tup/vect 0 0 1))
          shape (nth (:world/objects w) 1)
          i (intersection/intersection 0.5 shape)
          comps (comps/prepare-computations i ray)
          c (shade-hit w comps)]
      (is (color/color-eq? (color/color 0.90498 0.90498 0.90498) c))))

  (testing "Shade hit can handle a point that is in shadow"
    (let [s1 (shape/sphere)
          s2 (shape/apply-transform (shape/sphere) (xform/translation 0 0 10))
          w (-> empty-world
                (assoc :world/light
                       (light/point-light
                        (tup/point 0 0 -10) color/white))
                (update :world/objects conj s1)
                (update :world/objects conj s2))
          r (ray/ray (tup/point 0 0 5) (tup/vect 0 0 1))
          i (intersection/intersection 4 s2)
          comps (comps/prepare-computations i r)
          c (shade-hit w comps)]
      (is (color/color-eq? (color/color 0.1 0.1 0.1) c)))))

(deftest color-at-test
  (testing "The color when a ray misses is black"
    (let [ray (ray/ray (tup/point 0 0 -5) (tup/vect 0 1 0))]
      (is (color/color-eq? color/black (color-at default-world ray)))))

  (testing "The color when a ray hits is determined by the hit object"
    (let [ray (ray/ray (tup/point 0 0 -5) (tup/vect 0 0 1))]
      (is (color/color-eq? (color/color 0.38066 0.47583 0.2855)
                           (color-at default-world ray)))))

  (testing "The color with an intersection behind the ray"
    (let [w (-> default-world
                (assoc-in [:world/objects 0 :material :material/ambient] 1)
                (assoc-in [:world/objects 1 :material :material/ambient] 1))
          r (ray/ray (tup/point 0 0 0.75) (tup/vect 0 0 -1))]
      (is (color/color-eq? (get-in w [:world/objects 1 :material :material/color])
                           (color-at w r))))))

(deftest shadowed?-test
  (testing "There is no shadow when nothing is colinear with point and light"
    (is (not (shadowed? default-world (tup/point 0 10 0)))))

  (testing "The shadow when there is an object between point and light"
    (is (shadowed? default-world (tup/point 10 -10 10))))

  (testing "There is no shadow when object is behind the light"
    (is (not (shadowed? default-world (tup/point -20 20 -20)))))

  (testing "There is no shadow when object is behind the point"
    (is (not (shadowed? default-world (tup/point -2 2 -2))))))

(deftest reflected-color-test
  (testing "The reflected color for a non-reflective material is black"
    (let [ray (ray/ray (tup/point 0 0 0) (tup/vect 0 0 1))
          w (assoc-in default-world [:world/objects 1 :material :material/ambient] 1)
          shape (nth (:world/objects w) 1)
          i (intersection/intersection 1 shape)
          comps (comps/prepare-computations i ray)]
      (is (color/color-eq? color/black (reflected-color w comps)))))

  (let [shape (-> (shape/plane)
                  (assoc-in [:material :material/reflective] 0.5)
                  (shape/apply-transform (xform/translation 0 -1 0)))
        w (update default-world :object conj shape)
        rad-2 (Math/sqrt 2)
        ray (ray/ray (tup/point 0 0 -3) (tup/vect 0 (/ rad-2 -2) (/ rad-2 2)))
        i (intersection/intersection rad-2 shape)
        comps (comps/prepare-computations i ray)]
    (testing "The reflected color for a reflective material"
      (is (color/color-eq? (color/color 0.19033 0.23791 0.14274)
                           (reflected-color w comps))))

    (testing "Shade-hit with a reflective material"
      (is (color/color-eq? (color/color 0.87676 0.92434 0.82917)
                           (shade-hit w comps))))

    (testing "The reflected color at max recursive depth is black"
      (is (color/color-eq? color/black (reflected-color w comps 0)))))

  (testing "Mutually reflective surfaces does not cause infinite recursion"
    (let [lower-mirror (-> (shape/plane)
                           (assoc-in [:material :material/reflective] 1.0)
                           (shape/apply-transform (xform/translation 0 -1 0)))
          upper-mirror (-> (shape/plane)
                           (assoc-in [:material :material/reflective] 1.0)
                           (shape/apply-transform (xform/translation 0 1 0)))
          w (-> empty-world
                (assoc :world/light (light/point-light (tup/point 0 0 0) color/white))
                (update :world/objects conj lower-mirror)
                (update :world/objects conj upper-mirror))
          r (ray/ray (tup/point 0 0 0) (tup/vect 0 1 0))]
      (is (some? (color-at w r))))))

(deftest refracted-color-test
  (testing "The refracted color of an opaque surface is black"
    (let [w default-world
          shape (nth (:world/objects w) 0)
          r (ray/ray (tup/point 0 0 0) (tup/vect 0 1 0))
          xs (intersection/intersections
              (intersection/intersection 4 shape)
              (intersection/intersection 6 shape))
          comps (comps/prepare-computations (nth xs 0) r xs)]
      (is (color/color-eq? color/black (refracted-color w comps)))))

  (testing "The refracted color at the max recursive depth is black"
    (let [w (-> default-world
                (assoc-in [:world/objects 0 :material :material/transparency] 1.0)
                (assoc-in [:world/objects 0 :material :material/refractive-index] 1.5))
          shape (nth (:world/objects w) 0)
          r (ray/ray (tup/point 0 0 0) (tup/vect 0 1 0))
          xs (intersection/intersections
              (intersection/intersection 4 shape)
              (intersection/intersection 6 shape))
          comps (comps/prepare-computations (nth xs 0) r xs)]
      (is (color/color-eq? color/black (refracted-color w comps 0)))))

  (testing "The refracted color under total internal reflection is black"
    (let [w (-> default-world
                (assoc-in [:world/objects 0 :material :material/transparency] 1.0)
                (assoc-in [:world/objects 0 :material :material/refractive-index] 1.5))
          shape (nth (:world/objects w) 0)
          rad-2 (Math/sqrt 2)
          r (ray/ray (tup/point 0 0 (/ rad-2 2)) (tup/vect 0 1 0))
          xs (intersection/intersections
              (intersection/intersection (/ rad-2 -2) shape)
              (intersection/intersection (/ rad-2 2) shape))
          comps (comps/prepare-computations (nth xs 1) r xs)]
      (is (color/color-eq? color/black (refracted-color w comps)))))

  (testing "The refracted color with the a refracted ray"
    (let [w (-> default-world
                (assoc-in [:world/objects 0 :material :material/ambient] 1.0)
                (assoc-in [:world/objects 0 :material :material/pattern] test-pattern)
                (assoc-in [:world/objects 1 :material :material/transparency] 1.0)
                (assoc-in [:world/objects 1 :material :material/refractive-index] 1.5))
          a (nth (:world/objects w) 0)
          b (nth (:world/objects w) 1)
          r (ray/ray (tup/point 0 0 0.1) (tup/vect 0 1 0))
          xs (intersection/intersections
              (intersection/intersection -0.9899 a)
              (intersection/intersection -0.4899 b)
              (intersection/intersection 0.4899 b)
              (intersection/intersection 0.9899 a))
          comps (comps/prepare-computations (nth xs 2) r xs)]
      (is (color/color-eq? (color/color 0 0.99888 0.04722) (refracted-color w comps 5)))))

  (testing "Shade hit with a teransparent material"
    (let [glass-floor (-> (shape/plane)
                          (shape/apply-transform (xform/translation 0 -1 0))
                          (assoc-in [:material :material/transparency] 0.5)
                          (assoc-in [:material :material/refractive-index] 1.5))
          under-floor-ball (-> (shape/sphere)
                               (assoc-in [:material :material/color] color/red)
                               (assoc-in [:material :material/ambient] 0.5)
                               (shape/apply-transform (xform/translation 0 -3.5 -0.5)))
          w (-> default-world
                (update :world/objects conj glass-floor)
                (update :world/objects conj under-floor-ball))
          rad-2 (Math/sqrt 2)
          r (ray/ray (tup/point 0 0 -3) (tup/vect 0 (/ rad-2 -2) (/ rad-2 2)))
          xs (intersection/intersections
              (intersection/intersection rad-2 glass-floor))
          comps (comps/prepare-computations (nth xs 0) r xs)]
      (is (color/color-eq? (color/color 0.93642 0.68642 0.68642) (shade-hit w comps 5)))))

  (testing "Shade hit with a reflective, transparent surface"
    (let [glass-floor (-> (shape/plane)
                          (shape/apply-transform (xform/translation 0 -1 0))
                          (assoc-in [:material :material/reflective] 0.5)
                          (assoc-in [:material :material/transparency] 0.5)
                          (assoc-in [:material :material/refractive-index] 1.5))
          ball-under-floor (-> (shape/sphere)
                               (assoc-in [:material :material/color] color/red)
                               (assoc-in [:material :material/ambient] 0.5)
                               (shape/apply-transform (xform/translation 0 -3.5 -0.5)))
          w (-> default-world
                (update :world/objects conj glass-floor)
                (update :world/objects conj ball-under-floor))
          rad-2 (Math/sqrt 2)
          r (ray/ray (tup/point 0 0 -3) (tup/vect 0 (/ rad-2 -2) (/ rad-2 2)))
          xs (intersection/intersections
              (intersection/intersection rad-2 glass-floor))
          comps (comps/prepare-computations (nth xs 0) r xs)]
      (is (color/color-eq? (color/color 0.93391 0.69643 0.69243) (shade-hit w comps 5))))))
