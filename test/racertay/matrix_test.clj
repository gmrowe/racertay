(ns racertay.matrix-test
  (:require [clojure.test :refer :all]
            [racertay.matrix :refer :all]
            [racertay.fcmp :refer [nearly-eq?]]
            [racertay.tuple :refer [tuple tup-eq?]]))

(deftest create-matrix-test
  (testing "a 4x4 matrix can be constructed and queried"
    (let [mat (mat4x4
               1 2 3 4
               5.5 6.5 7.5 8.5
               9 10 11 12
               13.5 14.5 15.5 16.5)]
      (is (nearly-eq? 1 (mat-get mat 0 0)))
      (is (nearly-eq? 4 (mat-get mat 0 3)))
      (is (nearly-eq? 11 (mat-get mat 2 2)))))

  (testing  "a 2x2 matrix can be created and queried"
    (let [mat (mat2x2
               -3 5
               -1 2)]
      (is (nearly-eq? -3 (mat-get mat 0 0)))
      (is (nearly-eq? -1 (mat-get mat 1 0)))))

  (testing  "a 3x3 matrux can be created and queried"
    (let [mat (mat3x3
               -3 5 0
               1 -2 7
               0 1 1)]
      (is (nearly-eq? -2 (mat-get mat 1 1)))
      (is (nearly-zero? (mat-get mat 0 2))))))

(deftest matrix-eqaulity-test
  (testing  "equal matrices are equal"
    (let [mat1 (mat4x4
                1 2 3 4
                5 6 7 8
                9 8 7 6
                5 4 3 2)

          mat2 (mat4x4
                1 2 3 4
                5 6 7 8
                9 8 7 6
                5 4 3 2)]
      (is (mat-eq? mat1 mat2))))

  (testing "differtent matrices are not equal"
    (let [mat1 (mat4x4
                1 2 3 4
                5 6 7 8
                9 8 7 6
                5 4 3 2)

          mat2 (mat4x4
                5 6 7 8
                9 8 7 6
                5 4 3 2
                1 2 3 4)]
      (is (not (mat-eq? mat1 mat2))))))

(deftest matrix-multiplication-test
  (testing "two matrices can be multiplied"
    (let [mat1 (mat4x4
                1 2 3 4
                5 6 7 8
                9 8 7 6
                5 4 3 2)

          mat2 (mat4x4
                -2 1 2 3
                3 2 1 -1
                4 3 6 5
                1 2 7 8)

          expected (mat4x4
                    20 22 50 48
                    44 54 114 108
                    40 58 110 102
                    16 26 46 42)]
      (is (mat-eq? expected (mat-mul mat1 mat2)))))

  (testing  "a matrix can ve multiplied by a tuple"
    (let [mat (mat4x4
               1 2 3 4
               2 4 4 2
               8 6 4 1
               0 0 0 1)
          tup (tuple 1 2 3 1)]
      (is (tup-eq? (tuple 18 24 33 1) (mat-mul-tup mat tup))))))

(deftest identity-matrix-test
  (testing "multiplying matrix by the identity matrix leaves it unchanged"
    (let [mat (mat4x4
               0 1 2 4
               1 2 4 8
               2 4 8 16
               4 8 16 32)]
      (is (mat-eq? mat (mat-mul mat identity-matrix))))))

(deftest matrix-transposition-test
  (testing "a 4x4 matrix can be transposed" 
    (let [mat (mat4x4
               0 9 3 0
               9 8 0 8
               1 8 5 3
               0 0 5 8)

          expected (mat4x4
                    0 9 1 0
                    9 8 8 0
                    3 0 5 5
                    0 8 3 8)]
      (is (mat-eq? expected (transpose mat)))))

  (testing "transposing the identity matrix leaves it unchanged"
    (is (mat-eq? identity-matrix (transpose identity-matrix)))))

(deftest submatrix-test
  (testing "the submatric of a 4x4 matrix is a 3x3 matrix"
    (let [mat (mat4x4
               -6 1 1 6
               -8 5 8 6
               -1 0 8 2
               -7 1 -1 1)]
      (is (mat-eq? (mat3x3 -6 1 6
                           -8 8 6
                           -7 -1 1)
                   (submatrix mat 2 1)))))
  
  (testing "the submatrix of a 3x3 matrix is a 2x2 matrix"
    (let [mat (mat3x3
               1 5 0
               -3 2 7
               0 6 -3)]
      (is (mat-eq? (mat2x2 -3 2
                           0 6)
                   (submatrix mat 0 2))))))

(deftest minor-test
  (testing "the minor of an element of a 3x3 matrix can be determined"
    (let [mat (mat3x3
               3 5 0
               2 -1 -7
               6 -1 5)
          row 1
          col 0]
      (is (nearly-eq? 25 (minor-3x3 mat row col)))))

  (testing "the minor of an element of a 4x4 matrix can be detrermined"
    (let [mat (mat4x4
               -2 -8 3 5
               -3 1 7 3
               1 2 -9 6
               -6 7 7 -9)]
      (is (nearly-eq? 690 (minor mat 0 0)))
      (is (nearly-eq? -447 (minor mat 0 1))))))

(deftest cofactor-test
  (testing "the cofactor of an element of a 3x3 matrix can be determined"
    (let [mat (mat3x3
               3 5 0
               2 -1 -7
               6 -1 5)]
      (is (nearly-eq? -12 (cofactor-3x3 mat 0 0)))
      (is (nearly-eq? -25 (cofactor-3x3 mat 1 0)))))

  (testing "the cofactor of an element of a 4x4 matrix can be determined"
    (let [mat (mat4x4
               -2 -8 3 5
               -3 1 7 3
               1 2 -9 6
               -6 7 7 -9)]
      (is (nearly-eq? 690 (cofactor mat 0 0)))
      (is (nearly-eq? 447 (cofactor mat 0 1)))
      (is (nearly-eq? 210 (cofactor mat 0 2)))
      (is (nearly-eq? 51 (cofactor mat 0 3))))))

(deftest determinant-test
  (testing "the determinant of a 2x2 matrix can be calculated"
    (let [mat (mat2x2
               1  5
               -3 2)]
      (is (nearly-eq? 17 (determinant-2x2 mat)))))
  
  (testing "the determinant of a 3x3 matrix can be calculated"
    (let [mat (mat3x3
               1 2 6
               -5 8 -4
               2 6 4)]
      (is (nearly-eq? -196 (determinant-3x3 mat)))))

  (testing "the determinant of a 4x4 matrix can be calculated"
    (let [mat (mat4x4
               -2 -8 3 5
               -3 1 7 3
               1 2 -9 6
               -6 7 7 -9)]
      (is (nearly-eq? -4071 (determinant mat))))))

(deftest invert-test
  (testing "(invertable? mat) returns true if matrix is invertable"
    (let [mat (mat4x4
               6 4 4 4
               5 5 7 6
               4 -9 3 -7
               9 1 7 -6)]
      (is (invertable? mat))))

  (testing "(invertable? mat) returns false if matrix is not invertable"
    (let [mat (mat4x4
               -4 2 -2 -3
               9 6 2 6
               0 -5 1 -5
               0 0 0 0)]
      (is (not (invertable? mat)))))

  (testing "a 4x4 matrix can be inverted"
    (let [mat (mat4x4
               -5 2 6 -8
               1 -5 1 8
               7 7 -6 -7
               1 -3 7 4)
          expected (mat4x4
                    0.21805 0.45113 0.24060 -0.04511
                    -0.80827 -1.45677 -0.44361 0.52068
                    -0.07895 -0.22368 -0.05263 0.19737
                    -0.52256 -0.81391 -0.30075 0.30639)]
      (is (mat-eq? expected (inverse mat)))))

  (testing "a the inverse of a second 4x4 matrix"
    (let [mat (mat4x4
               8 -5 9 2
               7 5 6 1
               -6 0 9 6
               -3 0 -9 -4)
          expected (mat4x4
                    -0.15385 -0.15385 -0.28205 -0.53846 
                    -0.07692 0.12308 0.02564 0.03077
                    0.35897 0.35897 0.43590 0.92308
                    -0.69231 -0.69231 -0.76923 -1.92308)]
      (is (mat-eq? expected (inverse mat)))))

  (testing "a the inverse of a third 4x4 matrix"
    (let [mat (mat4x4
               9 3 0 9
               -5 -2 -6 -3
               -4 9 6 4
               -7 6 6 2)
          expected (mat4x4
                    -0.04074 -0.07778 0.14444 -0.22222
                    -0.07778 0.03333 0.36667 -0.33333
                    -0.02901 -0.14630 -0.10926 0.12963
                    0.17778 0.06667 -0.26667 0.33333)]
      (is (mat-eq? expected (inverse mat)))))

  (testing "multiplying a product by its inverse yields the original matrix"
    (let [mat1 (mat4x4
                8 -5 9 2
                7 5 6 1
                -6 0 9 6
                -3 0 -9 -4)
          mat2 (mat4x4
                9 3 0 9
                -5 -2 -6 -3
                -4 9 6 4
                -7 6 6 2)

          prod (mat-mul mat1 mat2)]
      (is (mat-eq? mat1 (mat-mul prod (inverse mat2)))))))
