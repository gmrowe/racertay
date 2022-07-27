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
            [racertay.fcmp :as fcmp]))

(def default-light (light/point-light (tup/point -10 10 -10) (color/color 1 1 1)))

(def sphere-1
  (let [material (-> (material/material)
                     (assoc :color (color/color 0.8 1.0 0.6))
                     (assoc :diffuse 0.7)
                     (assoc :specular 0.2))]
    (assoc (sphere/sphere) :material material)))

(def sphere-2
  (let [scale (xform/scaling 0.5 0.5 0.5)]
    (sphere/apply-transform (sphere/sphere) scale)))

(def default-world
  (-> empty-world
      (assoc :light default-light)
      (update :objects conj sphere-1)
      (update :objects conj sphere-2)))

(deftest world-creation-test
  (testing "A new world has no objects and no light source"
    (let [w empty-world]
      (is (zero? (count (:objects w))))
      (is (nil? (:light w))))))

(deftest default-world-test
  (testing "The default world has default light"
    (is (light/eq? default-light (:light default-world))))

  (testing "The default world containd sphere-1"
    (is (some? (some #{sphere-1} (:objects default-world)))))

  (testing "The defaule world containd sphere-2"
    (is (some? (some #{sphere-1} (:objects default-world))))))

(deftest intersect-world-test
  (testing "A ray can intersect with a world"
    (let [ray (ray/ray (tup/point 0 0 -5) (tup/vect 0 0 1))
          xs (intersect-world default-world ray)]
      (is (= 4 (count xs)))
      (is (fcmp/nearly-eq? 4 (:t (nth xs 0))))
      (is (fcmp/nearly-eq? 4.5 (:t (nth xs 1))))
      (is (fcmp/nearly-eq? 5.5 (:t (nth xs 2))))
      (is (fcmp/nearly-eq? 6 (:t (nth xs 3)))))))
