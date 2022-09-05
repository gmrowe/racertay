(ns racertay.shapes.cylinder
  (:require
   [clojure.math :as math]
   [racertay.fcmp :as fcmp]
   [racertay.intersection :as inter]
   [racertay.protocols :as p]
   [racertay.ray :as ray]
   [racertay.tuple :as tup]))

(defn- check-cylinder-cap? [ray t]
  (let [{:ray/keys [direction origin]} ray
        x (+ (tup/x origin) (* t (tup/x direction)))
        z (+ (tup/z origin) (* t (tup/z direction)))]
    (<= (+ (* x x) (* z z)) 1.0)))

(defn- intersect-cylinder-caps
  [cylinder ray]
  (let [{:keys [minimum maximum closed?]} cylinder
        direction-y (tup/y (:ray/direction ray))
        origin-y (tup/y (:ray/origin ray ))]
    (if (or (not closed?) (fcmp/nearly-zero? direction-y))
      inter/empty-intersections
      (let [t-lower (/ (- minimum origin-y) direction-y)
            t-upper (/ (- maximum origin-y) direction-y)]
        (reduce #(conj %1 (inter/intersection %2 cylinder))
                inter/empty-intersections
                (filter #(check-cylinder-cap? ray %) [t-lower t-upper]))))))

(defn- intersect-cylinder-sides
  [cylinder ray]
  (let [{:ray/keys [origin direction]} ray
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
                      (filter in-y-bounds? [t0 t1]))))))))

(defrecord ICylinder
    [minimum maximum]
  p/Shape
  (local-normal-at [cylinder local-point]
    (let [x (tup/x local-point)
          y (tup/y local-point)
          z (tup/z local-point)
          distance (+ (* x x) (* z z))]
      (cond
        (and (< distance 1.0) (<= (- (:maximum cylinder) fcmp/epsilon) y)) (tup/vect 0 1 0)
        (and (< distance 1.0) (<= y (+ (:minimum cylinder) fcmp/epsilon))) (tup/vect 0 -1 0)
        :else (tup/vect x 0 z))))

  (local-intersect [cylinder local-ray]
    (sort-by
     :intersection/t
     (concat (intersect-cylinder-sides cylinder local-ray)
             (intersect-cylinder-caps cylinder local-ray)))))
