(ns racertay.computations
  (:require [racertay.ray :as ray]
            [racertay.tuple :as tup]
            [racertay.shape :as shape]
            [racertay.fcmp :as fcmp]
            [racertay.intersection :as inter]))

(defn- calc-n1-and-n2 [intersection all-intersections]
  (let [f (fn [containers i]
            (let [hit? (= i intersection)
                  n1 (when hit?
                       (or
                        (get-in (last containers) [:material :material/refractive-index])
                        1.0))
                  object (:intersection/object i)
                  c (if (some #{object} containers)
                      (vec (remove #{object} containers))
                      (conj containers object))
                  n2  (when hit?
                        (or
                         (get-in (last c) [:material :material/refractive-index])
                          1.0))]
              (if hit?
                (reduced [n1 n2])
                c)))]
    (reduce f [] all-intersections)))

(defn prepare-computations
  ([inters ray]
   (prepare-computations inters ray (inter/intersections inters)))
  ([inters ray all-intersections]
   (let [point (ray/position ray (:intersection/t inters))
         normalv (shape/normal-at (:intersection/object inters) point)
         eyev (tup/tup-neg (:ray/direction ray))
         normalv-dot-eyev (tup/dot normalv eyev)
         inside (neg? normalv-dot-eyev)
         true-normalv (if inside (tup/tup-neg normalv) normalv)
         over-point (tup/tup-add
                     point (tup/tup-mul-scalar true-normalv fcmp/epsilon))
         under-point (tup/tup-sub
                      point (tup/tup-mul-scalar true-normalv fcmp/epsilon))
         reflectv (tup/reflect (:ray/direction ray) true-normalv)
         [n1 n2] (calc-n1-and-n2 inters all-intersections)]
     (merge inters
            #:intersection{:point point
                           :eyev eyev
                           :normalv true-normalv
                           :inside inside
                           :over-point over-point
                           :reflectv reflectv
                           :n1 n1
                           :n2 n2
                           :under-point under-point}))))

