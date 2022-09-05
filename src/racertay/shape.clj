(ns racertay.shape
  (:require
   [racertay.material :as material]
   [racertay.matrix :as matrix]
   [racertay.protocols :as p]
   [racertay.ray :as ray]
   [racertay.tuple :as tup]
   [racertay.shapes.cone :as cone]
   [racertay.shapes.cube :as cube]
   [racertay.shapes.cylinder :as cylinder]
   [racertay.shapes.plane :as plane]
   [racertay.shapes.sphere :as sphere]))

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

(defn cylinder
  ([] (cylinder ##-Inf ##Inf :open))
  ([minimum maximum closed?]
   (cylinder/map->ICylinder
    (merge
     (shape-data)
     {:minimum minimum
      :maximum maximum
      :closed? (= closed? :closed)}))))

(defn cone
  ([] (cone ##-Inf ##Inf :open))
  ([minimum maximum closed?]
   (cone/map->ICone
    (merge
     (shape-data)
     {:minimum minimum
      :maximum maximum
      :closed? (= closed? :closed)}))))
