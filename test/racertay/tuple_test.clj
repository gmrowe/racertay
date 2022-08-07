(ns racertay.tuple-test
  (:require [clojure.test :refer :all]
            [racertay.tuple :refer :all]
            [racertay.fcmp :refer :all]))

(deftest tuple-test
  (let [t (tuple 4.3 -4.2 3.1 0.0)]
    (testing "a tuple should have an :x key"
      (is (= (x t) 4.3)))

    (testing "a tuple should have a :y key"
      (is (= (y t) -4.2)))

    (testing "a tuple should have a :z key"
      (is (= (z t) 3.1)))

    (testing "a tuple should have a :w key"
      (is (= (w t) 0.0)))

    (testing "a tuple with w = 0.0 is a vect"
      (is (vect? t)))

    (testing "a tuple with w = 0.0 is not a point"
      (is (not (point? t)))))

  (let [t (tuple 4.3 -4.2 3.1 1.0)]
    (testing "a tuple with w = 1.0 is a point"
      (is (point? t)))

    (testing "a tuple with w = 1.0 is not a vect"
      (is (not (vect? t))))))

(deftest tup-eq?-test
  (testing "Identical tuples are equal"
    (is (tup-eq? (point 1 2 3) (point 1 2 3))))

  (testing "A point and a vec are not equal"
    (is (not (tup-eq? (point 1 2 3) (vect 1 2 3)))))

  (testing "Bugfix!! a tuple is not equal to an empty vec"
    (is (not (tup-eq? (point 1 2 3) []))))

  (testing  "tup-eq? can take more that two args and returns trus iff all are true"
    (is (tup-eq? (point 1 2 3) (point 1 2 3) (point 1 2 3)))
    (is (not (tup-eq? (point 1 2 3) (point 2 3 4) (point 1 2 3))))))

(deftest point-test
  (testing "a point creates a tuple with w=1.0"
    (is (tup-eq? (point 4 -4 3) (tuple 4 -4 3 1)))))

(deftest vector-test
  (testing "a vect creates a tuple with w=0.0"
    (is (tup-eq? (vect 4 -4 3) (tuple 4 -4 3 0)))))

(deftest tuple-addition-test 
  (testing "tuples can be added"
    (let [a1 (tuple 3 -2 5 1)
          a2 (tuple -2 3 1 0)]
      (is (tup-eq? (tuple 1 1 6 1) (tup-add a1 a2)))))

  (testing "tuple addition can take a variable number of args"
    (let [a1 (tuple 1 2 3 4)
          a2 (tuple 2 3 4 5)
          a3 (tuple 3 4 5 6)]
      (is (tup-eq? (tuple 6 9 12 15) (tup-add a1 a2 a3)))))

  (testing "tuple addition can take a single arg"
    (let [t (tuple 1 2 3 4)]
      (is (tup-eq? t (tup-add t))))))

(deftest tuple-subtraction-test 
  (testing "two points can be subtracted"
    (let [p1 (point 3 2 1)
          p2 (point 5 6 7)]
      (is (tup-eq? (vect -2 -4 -6) (tup-sub p1 p2)))))
  
  (testing "a vector can be subtracted from a point"
    (let [p (point 3 2 1)
          v (vect 5 6 7)]
      (is (tup-eq? (point -2 -4 -6) (tup-sub p v)))))

  (testing "two vectors can be subtracted"
    (let [v1 (vect 3 2 1)
          v2 (vect 5 6 7)]
      (is (tup-eq? (vect -2 -4 -6) (tup-sub v1 v2)))))

  (testing "tuple subtraction can take a variable number of args"
    (let [v1 (vect 10 9 8)
          v2 (vect 6 5 4)
          v3 (vect 3 2 1)]
      (is (tup-eq? (vect 1 2 3) (tup-sub v1 v2 v3)))))

  (testing "tuple subtraction can take a single arg"
    (let [t (tuple 1 -2 3 -4)]
      (is (tup-eq? (tuple -1 2 -3 4) (tup-sub t))))))

(deftest tuple-negation-test 
  (testing "a tuple can be negated"
    (let [t (tuple 1 -2 3 -4)]
      (is (tup-eq? (tuple -1 2 -3 4) (tup-neg t))))))

(deftest tuple-scalar-multiplication-test 
  (testing "a tuple can be multiplied by a scalar"
    (let [a (tuple 1 -2 3 -4)]
      (is (tup-eq? (tuple 3.5 -7 10.5 -14) (tup-mul-scalar a 3.5)))))

  (testing "a tuple can be multiplied by a fraction"
    (let [a (tuple 1 -2 3 -4)]
      (is (tup-eq? (tuple 0.5 -1 1.5 -2) (tup-mul-scalar a 0.5))))))

(deftest tuple-scalar-division-test 
  (testing "a tuple can be divided by a scalar"
    (let [a (tuple 1 -2 3 -4)]
      (is (tup-eq? (tuple 0.5 -1 1.5 -2) (tup-div-scalar a 2))))))

(deftest vector-magnitude-test 
  (testing "the magnitude of a vector can be computed"
    (is (nearly-eq? 1 (magnitude (vect 1 0 0))))
    (is (nearly-eq? 1 (magnitude (vect 0 1 0))))
    (is (nearly-eq? 1 (magnitude (vect 0 0 1))))
    (is (nearly-eq? (Math/sqrt 14) (magnitude (vect 1 2 3))))
    (is (nearly-eq? (Math/sqrt 14) (magnitude (vect -1 -2 -3))))))

(deftest vector-normalization-test 
  (testing "a vector can be normalized"
    (is (tup-eq? (vect 1 0 0) (normalize (vect 4 0 0))))
    (is (tup-eq? (vect 0.26726, 0.53452, 0.80178) (normalize (vect 1 2 3)))))
  
  (testing "the magnitude of a normalized vector is always 1"
    (let [v (vect 128.23 -0.223423 3.14159)
          norm (normalize v)]
      (is (nearly-eq? 1 (magnitude norm))))))

(deftest dot-product-test
  (testing "the dot product of two vectors can be computed"
    (let [a (vect 1 2 3)
          b (vect 2 3 4)]
      (is (nearly-eq? 20 (dot a b))))))

(deftest cross-product-test
  (testing "the cross product of two vectors can be computed"
    (let [a (vect 1 2 3)
          b (vect 2 3 4)]
      (is (tup-eq? (vect -1 2 -1) (cross a b)))
      (is (tup-eq? (vect 1 -2 1) (cross b a))))))

(deftest vector-reflection-test
  (testing "Reflecting a vector approaching at 45 degrees about a normal"
    (let [v (vect 1 -1 0)
          n (vect 0 1 0)]
      (is (tup-eq? (vect 1 1 0)(reflect v n)))))

  (testing "Reflecting a vector off a slanted surface"
    (let [v (vect 0 -1 0)
          rad-2-over-2 (/ (Math/sqrt 2) 2)
          n (vect rad-2-over-2 rad-2-over-2 0)]
      (is (tup-eq? (vect 1 0 0) (reflect v n))))))
