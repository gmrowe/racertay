(ns racertay.intersection-test
  (:require [clojure.test :refer :all]
            [racertay.intersection :refer :all]
            [racertay.shape :as shape]
            [racertay.fcmp :as fcmp]
            [racertay.ray :as ray]
            [racertay.tuple :as tup]
            [racertay.transformations :as xform]
            [racertay.computations :as comps]
            [racertay.protocols :as p]
            [racertay.matrix :as matrix]))

(deftest intersection-creation-test
  (testing "An intersection encapsulates a t an an object"
    (let [s (shape/sphere)
          i (intersection 3.5 s)]
      (is (fcmp/nearly-eq? 3.5 (:intersection/t i)))
      (is (= s (:intersection/object i))))))

(deftest intersection-aggregation-test
  (testing "Intersections should be able to be aggregated"
    (let [s (shape/sphere)
          i1 (intersection 1 s)
          i2 (intersection 2 s)
          xs (intersections i1 i2)]
      (is (= 2 (count xs)))
      (is (fcmp/nearly-eq? 1 (:intersection/t (nth xs 0))))
      (is (fcmp/nearly-eq? 2 (:intersection/t (nth xs 1)))))))

(deftest intersection-hit-test
  (testing "The hit when all intersections have positive t"
    (let [s (shape/sphere)
          i1 (intersection 1 s)
          i2 (intersection 2 s)
          xs (intersections i2 i1)
          i (hit xs)]
      (is (= i1 i))))

  (testing "The hit when some intersections have negative t"
    (let [s (shape/sphere)
          i1 (intersection -1 s)
          i2 (intersection 1 s)
          xs (intersections i2 i1)
          i (hit xs)]
      (is (= i2 i))))

  (testing "The hit when all intersectins have negative t"
    (let [s (shape/sphere)
          i1 (intersection -1 s)
          i2 (intersection -2 s)
          xs (intersections i2 i1)
          i (hit xs)]
      (is (nil? i))))

  (testing "The hit is always the lowest nonnegative intersection"
    (let [s (shape/sphere)
          i1 (intersection 5 s)
          i2 (intersection 7 s)
          i3 (intersection -3 s)
          i4 (intersection 2 s)
          xs (intersections i1 i2 i3 i4)
          i (hit xs)]
      (is (= i4 i)))))

(def glass-sphere
  (-> (shape/sphere)
      (assoc-in [:material :material/transparency] 1.0)
      (assoc-in [:material :material/refractive-index] 1.5)))

(deftest prepare-computations-test
  (testing "The state of an intersection is precomputed"
    (let [r (ray/ray (tup/point 0 0 -5) (tup/vect 0 0 1))
          shape (shape/sphere)
          i (intersection 4 shape)
          comps (comps/prepare-computations i r)]
      (is (fcmp/nearly-eq? (:intersection/t i) (:intersection/t comps)))
      (is (= (:intersection/object i) (:intersection/object comps)))
      (is (tup/tup-eq? (tup/point 0 0 -1) (:intersection/point comps)))
      (is (tup/tup-eq? (tup/vect 0 0 -1) (:intersection/normalv comps)))
      (is (tup/tup-eq? (tup/vect 0 0 -1) (:intersection/eyev comps)))))

  (testing "The :inside attribute is false when intersection occurs outside"
    (let [r (ray/ray (tup/point 0 0 -5) (tup/vect 0 0 1))
          shape (shape/sphere)
          i (intersection 4 shape)
          comps (comps/prepare-computations i r)]
      (is (false? (:intersection/inside comps)))))

  (testing "The :inside attribute is true when intersection occurs inside"
    (let [r (ray/ray (tup/point 0 0 0) (tup/vect 0 0 1))
          shape (shape/sphere)
          i (intersection 1 shape)
          comps (comps/prepare-computations i r)]
      (is (tup/tup-eq? (tup/point 0 0 1) (:intersection/point comps)))
      (is (tup/tup-eq? (tup/vect 0 0 -1) (:intersection/eyev comps)))
      (is (true? (:intersection/inside comps)))
      (is (tup/tup-eq? (tup/vect 0 0 -1) (:intersection/normalv comps)))))

  (testing "The :over-point attribute should offset the hit"
    (let [r (ray/ray (tup/point 0 0 -5) (tup/vect 0 0 1))
          shape (-> (shape/sphere)
                    (shape/apply-transform (xform/translation 0 0 1)))
          i (intersection 5 shape)
          comps (comps/prepare-computations i r)]
      (is (< (tup/z (:intersection/over-point comps)) (/ fcmp/epsilon 2)))
      (is (< (tup/z (:intersection/over-point comps))
             (tup/z (:intersection/point comps))))))

  (testing "When the intersection occurs inside :over-point shold be inside"
    (let [r (ray/ray (tup/point 0 0 0) (tup/vect 0 0 1))
          shape (shape/sphere)
          i (intersection 1 shape)
          comps (comps/prepare-computations i r)
          diff-z (- (tup/z (:intersection/over-point comps))
                    (tup/z (:intersection/point comps)))]
      (is (< diff-z (- (/ fcmp/epsilon 2))))
      (is (< (tup/z (:intersection/over-point comps))
             (tup/z (:intersection/point comps))))))

  (testing "The reflection vector should be precomputed"
    (let [rad-2 (Math/sqrt 2)
          shape (shape/plane)
          r (ray/ray (tup/point 0 1 -1) (tup/vect 0 (/ rad-2 -2) (/ rad-2 2)))
          i (intersection rad-2 shape)
          comps (comps/prepare-computations i r)]
      (is (tup/tup-eq? (tup/vect 0 (/ rad-2 2) (/ rad-2 2))
                       (:intersection/reflectv comps)))))

  (testing "n1 and n2 should be precomputed"
    (testing "for a multi-sphere scenario" 
      (let [a (shape/apply-transform glass-sphere (xform/scaling 2 2 2))
            b (-> glass-sphere
                  (shape/apply-transform (xform/translation 0 0 -0.25))
                  (assoc-in [:material :material/refractive-index] 2.0))
            c (-> glass-sphere
                  (shape/apply-transform (xform/translation 0 0 0.25))
                  (assoc-in [:material :material/refractive-index] 2.5))
            ray (ray/ray (tup/point 0 0 -4) (tup/vect 0 0 1))
            xs (intersections
                (intersection 2 a)
                (intersection 2.75 b)
                (intersection 3.25 c)
                (intersection 4.25 b)
                (intersection 5.25 c)
                (intersection 6 a))]
        (testing "at intersection[0]"
          (let [comps (comps/prepare-computations (nth xs 0) ray xs)]
            (is (fcmp/nearly-eq? 1.0 (:intersection/n1 comps)))
            (is (fcmp/nearly-eq? 1.5 (:intersection/n2 comps)))))

        (testing "at intersection[1]"
          (let [comps (comps/prepare-computations (nth xs 1) ray xs)]
            (is (fcmp/nearly-eq? 1.5 (:intersection/n1 comps)))
            (is (fcmp/nearly-eq? 2.0 (:intersection/n2 comps)))))

        (testing "at intersection[2]"
          (let [comps (comps/prepare-computations (nth xs 2) ray xs)]
            (is (fcmp/nearly-eq? 2.0 (:intersection/n1 comps)))
            (is (fcmp/nearly-eq? 2.5 (:intersection/n2 comps)))))

        (testing "at intersection[3]"
          (let [comps (comps/prepare-computations (nth xs 3) ray xs)]
            (is (fcmp/nearly-eq? 2.5 (:intersection/n1 comps)))
            (is (fcmp/nearly-eq? 2.5 (:intersection/n2 comps)))))

        (testing "at intersection[4]"
          (let [comps (comps/prepare-computations (nth xs 4) ray xs)]
            (is (fcmp/nearly-eq? 2.5 (:intersection/n1 comps)))
            (is (fcmp/nearly-eq? 1.5 (:intersection/n2 comps)))))

        (testing "at intersection[5]"
          (let [comps (comps/prepare-computations (nth xs 5) ray xs)]
            (is (fcmp/nearly-eq? 1.5 (:intersection/n1 comps)))
            (is (fcmp/nearly-eq? 1.0 (:intersection/n2 comps))))))))

  (testing "under-point should be precomputed"
    (let [ray (ray/ray (tup/point 0 0 -5) (tup/vect 0 0 1))
          shape (shape/apply-transform glass-sphere (xform/translation 0 0 1))
          i (intersection 5 shape)
          comps (comps/prepare-computations i ray)]
      (is (< (/ fcmp/epsilon 2)  (tup/z (:intersection/under-point comps))))
      (is (< (tup/z (:intersection/point comps)) (tup/z (:intersection/under-point comps)))))))

(deftest schlick-approximation-test
  (testing "The shlick approximation under total internal reflection is 1.0"
    (let [shape glass-sphere
          rad-2 (Math/sqrt 2)
          r (ray/ray (tup/point 0 0 (/ rad-2 2)) (tup/vect 0 1 0))
          xs (intersections
              (intersection (/ rad-2 -2) shape)
              (intersection (/ rad-2 2) shape))
          comps (comps/prepare-computations (nth xs 1) r xs)]
      (is (fcmp/nearly-eq? 1.0 (schlick comps)))))

  (testing "The schlick approximation from a perpendicular viewing angle"
    (let [shape glass-sphere
          r (ray/ray (tup/point 0 0 0) (tup/vect 0 1 0))
          xs (intersections
              (intersection -1 shape)
              (intersection 1 shape))
          comps (comps/prepare-computations (nth xs 1) r xs)]
      (is (fcmp/nearly-eq? 0.04 (schlick comps)))))

  (testing "The schlick approximation with a small angle and n2 > n1"
    (let [shape glass-sphere
          r (ray/ray (tup/point 0 0.99 -2) (tup/vect 0 0 1))
          xs (intersections
              (intersection 1.8589 shape))
          comps (comps/prepare-computations (nth xs 0) r xs)]
      (is (fcmp/nearly-eq? 0.48873 (schlick comps))))))
