(ns racertay.camera-test
  (:require [clojure.test :refer :all]
            [racertay.camera :refer :all]
            [racertay.matrix :as mat]
            [racertay.fcmp :as fcmp]
            [racertay.tuple :as tup]
            [racertay.ray :as ray]
            [racertay.transformations :as xform]
            [racertay.world-test :as wt]
            [racertay.canvas :as can]
            [racertay.color :as col]))

(deftest camera-creation-test
  (testing "A camera is constructed from from hsize vsoze and field-of-view"
    (let [hsize 160
          vsize 120
          field-of-view (/ (Math/PI) 2)
          c (camera hsize vsize field-of-view)]
      (is (= hsize (:camera/hsize c)))
      (is (= vsize (:camera/vsize c)))
      (is (= field-of-view (:camera/field-of-view c)))
      (is (mat/mat-eq? mat/identity-matrix (:camera/transform c))))))

(deftest pixel-size-test
  (testing "The pixel size can for a horizontal canvas can be calculated"
    (let [c (camera 200 125 (/ Math/PI 2))]
      (is (fcmp/nearly-eq? 0.01 (:camera/pixel-size c)))))

  (testing "The pixel size can for a vertical canvas van be calculated"
    (let [c (camera 125 200 (/ Math/PI 2))]
      (is (fcmp/nearly-eq? 0.01 (:camera/pixel-size c))))))

(deftest ray-for-pixel-test
  (testing "Constructing a ray through the center of the canvas"
    (let [c (camera 201 101 (/ Math/PI 2))
          r (ray-for-pixel c 100 50)]
      (is (tup/tup-eq? (tup/point 0 0 0) (:ray/origin r)))
      (is (tup/tup-eq? (tup/vect 0 0 -1) (:ray/direction r)))))

  (testing "Construing a ray through the corner of the canvas"
    (let [c (camera 201 101 (/ Math/PI 2))
          r (ray-for-pixel c 0 0)]
      (is (tup/tup-eq? (tup/point 0 0 0) (:ray/origin r)))
      (is (tup/tup-eq? (tup/vect 0.66519 0.33259 -0.66851) (:ray/direction r)))))

  (testing "Constructing a ray when the camera is transformed"
    (let [transform (mat/mat-mul (xform/rotation-y (/ Math/PI 4))
                                 (xform/translation 0 -2 5)) 
          c (apply-transform (camera 201 101 (/ Math/PI 2)) transform)
          r (ray-for-pixel c 100 50)
          rad-2-over-2 (/ (Math/sqrt 2) 2)]
      (is (tup/tup-eq? (tup/point 0 2 -5) (:ray/origin r)))
      (is (tup/tup-eq? (tup/vect rad-2-over-2 0 (- rad-2-over-2))
                       (:ray/direction r))))))

(deftest render-test
  (testing "The a world can be rendered with a camera"
    (let [from (tup/point 0 0 -5)
          to (tup/point 0 0 0)
          up (tup/vect 0 1 0)
          c (apply-transform
             (camera 11 11 (/ Math/PI 2)) (xform/view-transform from to up))
          image (render c wt/default-world)]
      (is (col/color-eq? (col/color 0.38066 0.47583 0.2855)
                         (can/pixel-at image 5 5))))))

