(ns racertay.shape
  (:require [clojure.math :as math]
            [racertay.ray :as ray]
            [racertay.tuple :as tup]
            [racertay.fcmp :as fcmp]
            [racertay.intersection :as inter]
            [racertay.matrix :as matrix]
            [racertay.material :as material]
            [racertay.protocols :as p]
            [racertay.color :as color]))

(def world-origin (tup/point 0 0 0))

(defn shape-data []
  {:id (java.util.UUID/randomUUID)
   :transform matrix/identity-matrix
   :inverse-transform matrix/identity-matrix
   :material material/default-material})

(defn apply-transform
  ([obj xform]
   (let [updated (update obj :transform (partial matrix/mat-mul xform))]
     (assoc updated :inverse-transform (matrix/inverse (:transform updated)))))
  
  ([obj xform & more]
   (reduce apply-transform obj (cons xform more))))

(defn intersect [shape ray]
  (let [local-ray (ray/transform ray (:inverse-transform shape))]
    (p/local-intersect shape local-ray)))

(defn normal-at [shape point]
  (let [{:keys [inverse-transform]} shape
        local-point (matrix/mat-mul-tup inverse-transform point)
        local-normal (p/local-normal-at shape local-point)
        world-normal (matrix/mat-mul-tup
                      (matrix/transpose inverse-transform) local-normal)]
    (tup/normalize
     (tup/vect (tup/x world-normal) (tup/y world-normal) (tup/z world-normal)))))

(defrecord ISphere
    []
  p/Shape
  (local-normal-at [sphere local-point]
    (tup/tup-sub local-point world-origin))

  (local-intersect [sphere local-ray] 
    (let [{:ray/keys [origin direction]} local-ray
          sphere-to-ray-vec (tup/tup-sub origin world-origin)
          a (tup/dot direction direction)
          b (* 2 (tup/dot direction sphere-to-ray-vec))
          c (dec (tup/dot sphere-to-ray-vec sphere-to-ray-vec))
          discriminant (- (* b b) (* 4 a c))]
      (if (neg? discriminant)
        inter/empty-intersections
        (inter/intersections
         (inter/intersection (/ (- (- b) (math/sqrt discriminant)) (* a 2)) sphere)
         (inter/intersection (/ (+ (- b) (math/sqrt discriminant)) (* a 2)) sphere))))))

(defn sphere []
  (map->ISphere (shape-data)))

(defrecord IPlane
     [id transform inverse-transform material]
  p/Shape
  
  (local-normal-at [plane local-point]
    (tup/vect 0 1 0))
  
  (local-intersect [plane local-ray]
    (let [{:ray/keys [origin direction]} local-ray]
      (if (< fcmp/epsilon (abs (tup/y direction)))
        (let [t (/ (- (tup/y origin)) (tup/y direction))]
          (inter/intersections (inter/intersection t plane)))
        inter/empty-intersections))))

(defn plane []
  (map->IPlane (shape-data)))

(defrecord ICube
    [id transform inverse-transform material]
  p/Shape
  (local-normal-at [cube local-point]
    (let [x (tup/x local-point)
          y (tup/y local-point)
          z (tup/z local-point)
          maxc (max (abs x) (abs y) (abs z))]
      (condp = maxc
        (abs x) (tup/vect x 0 0)
        (abs y) (tup/vect 0 y 0)
        (abs z) (tup/vect 0 0 z))))
  
  (local-intersect [cube local-ray]
    (letfn [(check-axis [origin direction]
              (let [tmin-num (- -1 origin)
                    tmax-num (- 1 origin)]
                (sort
                 (if (< fcmp/epsilon (abs direction))
                   [(/ tmin-num direction) (/ tmax-num direction)]
                   [(* tmin-num ##Inf) (* tmax-num ##Inf)]))))]
      
      (let [{:ray/keys [origin direction]} local-ray
            [xtmin xtmax] (check-axis (tup/x origin) (tup/x direction))
            [ytmin ytmax] (check-axis (tup/y origin) (tup/y direction))
            [ztmin ztmax] (check-axis (tup/z origin) (tup/z direction))
            tmin (max xtmin ytmin ztmin)
            tmax (min xtmax ytmax ztmax)]
        (if (<= tmin tmax)
          (inter/intersections
           (inter/intersection tmin cube)
           (inter/intersection tmax cube))
          inter/empty-intersections)))))

(defn cube []
  (map->ICube (shape-data)))


(defrecord ICylinder
    [minimum maximum]
  p/Shape
  (local-normal-at [cylinder local-point]
    (tup/vect (tup/x local-point) 0.0 (tup/z local-point)))

  (local-intersect [cylinder local-ray]
    (let [{:ray/keys [origin direction]} local-ray
          direction-x (tup/x direction)
          direction-z (tup/z direction)
          a (+ (* direction-x direction-x) (* direction-z direction-z))]
      (if (fcmp/nearly-zero? a)
        inter/empty-intersections
        (let [origin-x (tup/x origin)
              origin-z (tup/z origin)
              b (+ (* 2.0 origin-x direction-x) (* 2.0 origin-z direction-z))
              c (+ (* origin-x origin-x) (* origin-z origin-z) -1.0)
              discriminant (- (* b b) (* 4.0 a c))]
          (if (neg? discriminant)
            inter/empty-intersections
            (let [t0 (/ (- (- b) (math/sqrt discriminant)) (* 2.0 a))
                  t1 (/ (+ (- b) (math/sqrt discriminant)) (* 2.0 a))
                  in-y-bounds? (fn [t]
                                 (let [y (+ (* t (tup/y direction)) (tup/y origin))]
                                   (< (:minimum cylinder) y (:maximum cylinder))))]
              (reduce #(conj %1 (inter/intersection %2 cylinder))
                      inter/empty-intersections
                      (filter in-y-bounds? [t0 t1])))))))))

(defn cylinder []
  (map->ICylinder
   (merge
    (shape-data)
    {:minimum ##-Inf
     :maximum ##Inf})))
