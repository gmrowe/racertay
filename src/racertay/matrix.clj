(ns racertay.matrix
  (:require [racertay.fcmp :refer [nearly-eq?]]
            [racertay.tuple :refer [tuple]]))

(defn- build-square-matrix [dim data]
  {:width dim
   :height dim
   :data (mapv double data) })

(defn mat2x2 [m00 m01
              m10 m11]
  (build-square-matrix
   2
   [m00 m01
    m10 m11]))

(defn mat3x3
  [m00 m01 m02
   m10 m11 m12
   m30 m31 m32]
  (build-square-matrix
   3
   [m00 m01 m02
    m10 m11 m12
    m30 m31 m32]))

(defn mat4x4
  [m00 m01 m02 m03
   m10 m11 m12 m13
   m20 m21 m22 m23
   m30 m31 m32 m33]
  (build-square-matrix
   4
   [m00 m01 m02 m03
    m10 m11 m12 m13
    m20 m21 m22 m23
    m30 m31 m32 m33]))

(def zero-matrix
  (mat4x4
   0 0 0 0
   0 0 0 0
   0 0 0 0
   0 0 0 0))

(def identity-matrix
  (mat4x4
   1 0 0 0
   0 1 0 0
   0 0 1 0
   0 0 0 1))

(defn- raw-index [mat row col]
  (+ col (* row (:width mat))))

(defn mat-get [mat row col]
  (get-in mat [:data (raw-index mat row col)]))

(defn mat-assoc [mat row col val]
  (assoc-in mat [:data (raw-index mat row col)] (double val)))

(defn mat-eq? [mat1 mat2]
  (and
   (= (:width mat1) (:width mat2))
   (= (:height mat1 (:height mat2)))
   (every? identity (map nearly-eq? (:data mat1) (:data mat2)))))

(defn mat-row [mat row]
  (for [col (range 0 (:width mat))]
    (mat-get mat row col)))

(defn mat-col [mat col]
  (for [row (range 0 (:height mat))]
    (mat-get mat row col)))

(defn mat-mul
  ([mat] mat)
  ([mat1 mat2]
   (apply mat4x4
          (for [r (range 0 (:height mat1))
                c (range 0 (:width mat2))]
            (apply + (map * (mat-row mat1 r) (mat-col mat2 c))))))
  ([mat1 mat2 & more]
   (reduce mat-mul (mat-mul mat1 mat2) more)))

(defn mat-mul-tup [mat tup]
  (apply tuple
         (for [row (range 0 (:height mat))]
           (apply + (map * tup (mat-row mat row))))))

(defn transpose [mat]
  (apply mat4x4 (mapcat #(mat-col mat %) (range 0 (:width mat)))))

(defn submatrix [mat row col]
  (build-square-matrix
   (dec (:width mat))
   (for [r (range 0 (:height mat))
         c (range 0 (:width mat))
         :when (and (not= r row)
                    (not= c col))]
     (mat-get mat r c))))

(defn determinant-2x2 [mat]
  (- (* (mat-get mat 0 0) (mat-get mat 1 1))
     (* (mat-get mat 0 1) (mat-get mat 1 0))))

(defn minor-3x3 [mat row col]
  (determinant-2x2 (submatrix mat row col)))

(defn cofactor-3x3 [mat row col]
  (let [factor (if (even? (+ row col)) 1 -1)]
    (* factor (minor-3x3 mat row col))))

(defn determinant-3x3 [mat]
  (let [f (fn [det col]
            (+ det (* (mat-get mat 0 col) (cofactor-3x3 mat 0 col))))]
    (reduce f 0 (range 0 (:width mat)))))

(defn minor [mat row col]
  (determinant-3x3 (submatrix mat row col)))

(defn cofactor [mat row col]
  (let [factor (if (even? (+ row col)) 1 -1)]
    (* factor (minor mat row col))))

(defn determinant [mat]
  (let [f (fn [det col]
            (+ det (* (mat-get mat 0 col) (cofactor mat 0 col))))]
    (reduce f 0 (range 0 (:width mat)))))

(defn invertable? [mat]
  (not (zero? (determinant mat))))

(defn inverse [mat]
  (let [det (determinant mat)]
    (when (not (zero? det))
      (let [invert-element (fn [m [r c]]
                             (mat-assoc m c r (/ (cofactor mat r c) det)))
            indices (for [row (range 0 (:height mat))
                          col (range 0 (:width mat))]
                      [row col])]
        (reduce invert-element zero-matrix indices)))))
