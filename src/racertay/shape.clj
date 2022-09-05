(ns racertay.shape
  (:require
   [racertay.shapes.cube :as cube]
   [racertay.shapes.cylinder :as cylinder]
   [racertay.shapes.plane :as plane]
   [racertay.shapes.sphere :as sphere]
   [clojure.math :as math]
            [racertay.ray :as ray]
            [racertay.tuple :as tup]
            [racertay.fcmp :as fcmp]
            [racertay.intersection :as inter]
            [racertay.matrix :as matrix]
            [racertay.material :as material]
            [racertay.protocols :as p]
            [racertay.color :as color]))

(defn shape-data []
  {:id (java.util.UUID/randomUUID)
   :transform matrix/identity-matrix
   :inverse-transform matrix/identity-matrix
   :material material/default-material})

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

(defn sphere []
  (sphere/map->ISphere (shape-data)))

(defn plane []
  (plane/map->IPlane (shape-data)))

(defn cube []
  (cube/map->ICube (shape-data)))

;; (defn- check-cylinder-cap? [ray t]
;;   (let [{:ray/keys [direction origin]} ray
;;         x (+ (tup/x origin) (* t (tup/x direction)))
;;         z (+ (tup/z origin) (* t (tup/z direction)))]
;;     (<= (+ (* x x) (* z z)) 1.0)))

;; (defn- intersect-cylinder-caps
;;   [cylinder ray]
;;   (let [{:keys [minimum maximum closed?]} cylinder
;;         direction-y (tup/y (:ray/direction ray))
;;         origin-y (tup/y (:ray/origin ray ))]
;;     (if (or (not closed?) (fcmp/nearly-zero? direction-y))
;;       inter/empty-intersections
;;       (let [t-lower (/ (- minimum origin-y) direction-y)
;;             t-upper (/ (- maximum origin-y) direction-y)]
;;         (reduce #(conj %1 (inter/intersection %2 cylinder))
;;                 inter/empty-intersections
;;                 (filter #(check-cylinder-cap? ray %) [t-lower t-upper]))))))

;; (defn- intersect-cylinder-sides
;;   [cylinder ray]
;;   (let [{:ray/keys [origin direction]} ray
;;           direction-x (tup/x direction)
;;           direction-z (tup/z direction)
;;           a (+ (* direction-x direction-x) (* direction-z direction-z))]
;;       (if (fcmp/nearly-zero? a)
;;         inter/empty-intersections
;;         (let [origin-x (tup/x origin)
;;               origin-z (tup/z origin)
;;               b (+ (* 2.0 origin-x direction-x) (* 2.0 origin-z direction-z))
;;               c (+ (* origin-x origin-x) (* origin-z origin-z) -1.0)
;;               discriminant (- (* b b) (* 4.0 a c))]
;;           (if (neg? discriminant)
;;             inter/empty-intersections
;;             (let [t0 (/ (- (- b) (math/sqrt discriminant)) (* 2.0 a))
;;                   t1 (/ (+ (- b) (math/sqrt discriminant)) (* 2.0 a))
;;                   in-y-bounds? (fn [t]
;;                                  (let [y (+ (* t (tup/y direction)) (tup/y origin))]
;;                                    (< (:minimum cylinder) y (:maximum cylinder))))]
;;               (reduce #(conj %1 (inter/intersection %2 cylinder))
;;                       inter/empty-intersections
;;                       (filter in-y-bounds? [t0 t1]))))))))

;; (defrecord ICylinder
;;     [minimum maximum]
;;   p/Shape
;;   (local-normal-at [cylinder local-point]
;;     (let [x (tup/x local-point)
;;           y (tup/y local-point)
;;           z (tup/z local-point)
;;           distance (+ (* x x) (* z z))]
;;       (cond
;;         (and (< distance 1.0) (<= (- (:maximum cylinder) fcmp/epsilon) y)) (tup/vect 0 1 0)
;;         (and (< distance 1.0) (<= y (+ (:minimum cylinder) fcmp/epsilon))) (tup/vect 0 -1 0)
;;         :else (tup/vect x 0 z))))

;;   (local-intersect [cylinder local-ray]
;;     (sort-by
;;      :intersection/t
;;      (concat (intersect-cylinder-sides cylinder local-ray)
;;              (intersect-cylinder-caps cylinder local-ray)))))

(defn cylinder
  ([] (cylinder ##-Inf ##Inf :open))
  ([minimum maximum closed?]
   (cylinder/map->ICylinder
    (merge
     (shape-data)
     {:minimum minimum
      :maximum maximum
      :closed? (= closed? :closed)}))))

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

(defn cone
  ([] (cone ##-Inf ##Inf :open))
  ([minimum maximum closed?]
   (map->ICone
    (merge
     (shape-data)
     {:minimum minimum
      :maximum maximum
      :closed? (= closed? :closed)}))))

(let [shape (cone -1 1 :closed)]
  (intersect-cone-caps shape (ray/ray (tup/point 0 -2 0) (tup/vect 0 1 0))))
