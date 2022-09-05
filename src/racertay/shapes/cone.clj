(ns racertay.shapes.cone
  (:require
   [clojure.math :as math]
   [racertay.fcmp :as fcmp]
   [racertay.intersection :as inter]
   [racertay.protocols :as p]
   [racertay.ray :as ray]
   [racertay.tuple :as tup]))

(defn- intersect-cone-sides
  [cone ray]
  (let [{:ray/keys [origin direction]} ray
          ori-x (tup/x origin)
          ori-y (tup/y origin)
          ori-z (tup/z origin)
          dir-x (tup/x direction)
          dir-y (tup/y direction)
          dir-z (tup/z direction)
          a (+ (* dir-x dir-x) (- (* dir-y dir-y)) (* dir-z dir-z))
          b (+ (* 2 ori-x dir-x) (- (* 2 ori-y dir-y)) (* 2 ori-z dir-z))
          c (+ (* ori-x ori-x) (- (* ori-y ori-y)) (* ori-z ori-z))]
      (if (fcmp/nearly-zero? a)
        (inter/intersections (inter/intersection (/ (- c) (* 2 b)) cone))
        (let [discriminant (- (* b b) (* 4.0 a c))]
          (if (neg? discriminant)
            inter/empty-intersections
            (let [t0 (/ (- (- b) (math/sqrt discriminant)) (* 2.0 a))
                  t1 (/ (+ (- b) (math/sqrt discriminant)) (* 2.0 a))
                  in-y-bounds? (fn [t]
                                 (let [y (+ (* t (tup/y direction)) (tup/y origin))]
                                   (< (:minimum cone) y (:maximum cone))))]
              (reduce #(conj %1 (inter/intersection %2 cone))
                      inter/empty-intersections
                      (filter in-y-bounds? [t0 t1]))))))))

(defn- check-cone-cap? [ray t y]
  (let [{:ray/keys [direction origin]} ray
        x (+ (tup/x origin) (* t (tup/x direction)))
        z (+ (tup/z origin) (* t (tup/z direction)))]
    (<= (+ (* x x) (* z z)) (abs y))))

(defn- intersect-cone-caps
  [cone ray]
  (let [{:keys [minimum maximum closed?]} cone
        direction-y (tup/y (:ray/direction ray))
        origin-y (tup/y (:ray/origin ray ))]
    (if (or (not closed?) (fcmp/nearly-zero? direction-y))
      inter/empty-intersections
      (let [t-lower (/ (- minimum origin-y) direction-y)
            t-upper (/ (- maximum origin-y) direction-y)
            xs0 inter/empty-intersections
            xs1 (if (check-cone-cap? ray t-lower minimum)
                  (conj xs0 (inter/intersection t-lower cone))
                  xs0)
            xs2 (if (check-cone-cap? ray t-upper maximum)
                  (conj xs1 (inter/intersection t-upper cone))
                  xs1)]
        xs2))))

(defrecord ICone
  [minimum maximum closed?]
  p/Shape
  (local-normal-at [cone local-point]
    (let [x (tup/x local-point)
          y (tup/y local-point)
          z (tup/z local-point)
          distance (+ (* x x) (* z z))
          vy (* (- (math/signum y)) (math/sqrt distance))]
      (cond
        (and (< distance (abs y)) (<= (- (:maximum cone) fcmp/epsilon) y)) (tup/vect 0 1 0)
        (and (< distance (abs y)) (<= y (+ (:minimum cone) fcmp/epsilon))) (tup/vect 0 -1 0)
        :else (tup/vect x vy z))))

  (local-intersect
    [cone local-ray]
    (sort-by
     :intersection/t
     (concat (intersect-cone-sides cone local-ray)
             (intersect-cone-caps cone local-ray)))))
