(ns racertay.world-test
  (:require [clojure.test :refer :all]
            [racertay.world :refer :all]
            [racertay.light :as light]
            [racertay.tuple :as tup]
            [racertay.color :as color]
            [racertay.sphere :as sphere]
            [racertay.material :as material]
            [racertay.transformations :as xform]
            [racertay.ray :as ray]
            [racertay.fcmp :as fcmp]
            [racertay.intersection :as intersection]))

(def default-light (light/point-light (tup/point -10 10 -10) (color/color 1 1 1)))

(def sphere-1
  (let [material (-> material/new-material
                     (assoc :material/color (color/color 0.8 1.0 0.6))
                     (assoc :material/diffuse 0.7)
                     (assoc :material/specular 0.2))]
    (assoc (sphere/sphere) :material material)))

(def sphere-2
  (let [scale (xform/scaling 0.5 0.5 0.5)]
    (sphere/apply-transform (sphere/sphere) scale)))

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

  (testing "The defaule world containd sphere-2"
    (is (some? (some #{sphere-1} (:world/objects default-world))))))

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
          comps (intersection/prepare-computations i ray)
          c (shade-hit default-world comps)]
      (is (color/color-eq? (color/color 0.38066 0.47583 0.2855) c))))

  (testing "An intersection on the inside can be shaded"
    (let [light (light/point-light (tup/point 0 0.25 0) (color/color 1 1 1))
          w (assoc default-world :world/light light)
          ray (ray/ray (tup/point 0 0 0) (tup/vect 0 0 1))
          shape (nth (:world/objects w) 1)
          i (intersection/intersection 0.5 shape)
          comps (intersection/prepare-computations i ray)
          c (shade-hit w comps)]
      (is (color/color-eq? (color/color 0.90498 0.90498 0.90498) c)))))

(deftest color-at-test
  (testing "The color when a ray misses is black"
    (let [ray (ray/ray (tup/point 0 0 -5) (tup/vect 0 1 0))]
      (is (color/color-eq? (color/color 0 0 0) (color-at default-world ray)))))

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
