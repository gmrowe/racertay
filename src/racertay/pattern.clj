(ns racertay.pattern
  (:require [racertay.color :as color]
            [racertay.tuple :as tup]
            [racertay.matrix :as matrix]
            [racertay.transformations :as xform]
            [racertay.protocols :as p]))

(def pattern-data
  {:transform matrix/identity-matrix
   :inverse-transform matrix/identity-matrix})

(defrecord IStripePattern
    [transform inverse-transform a b]
  p/Pattern
  (pattern-at [pattern point]
    (if (even? (int (Math/floor (tup/x point))))
      (:a pattern)
      (:b pattern))))

(defn stripe-pattern [color-a color-b]
  (map->IStripePattern
   (merge pattern-data {:a color-a :b color-b})))

(defrecord IGradientPattern
    [transform inverse-transform a b]
  p/Pattern
  (pattern-at [pattern point]
    (let [{:keys [a b]} pattern
          px (tup/x point)
          b-comp (color/color-mul-scalar
                  (color/color-sub b a) (- px (Math/floor px)))]
      (color/color-add a b-comp))))

(defn gradient-pattern [color-a color-b]
  (map->IGradientPattern
   (merge pattern-data {:a color-a :b color-b})))

(defrecord IRingPattern
    [transform inverse-transform a b]
  p/Pattern
  (pattern-at [pattern point]
    (let [px (tup/x point)
          pz (tup/z point)
          xz-distance-from-origin (Math/sqrt (+ (* px px) (* pz pz)))]
      (if (zero? (mod (int (Math/floor xz-distance-from-origin)) 2))
        (:a pattern)
        (:b pattern)))))

(defn ring-pattern [color-a color-b]
  (map->IRingPattern
   (merge pattern-data {:a color-a :b color-b})))

(defrecord ICheckerPattern
    [transform inverse-transform a b]
  p/Pattern
  (pattern-at [pattern point]
    (let [x (int (Math/floor (tup/x point)))
          y (int (Math/floor (tup/y point)))
          z (int (Math/floor (tup/z point)))]
      (if (zero? (mod (+ x y z) 2))
        (:a pattern)
        (:b pattern)))))

(defn checker-pattern [color-a color-b]
  (map->ICheckerPattern
   (merge pattern-data {:a color-a :b color-b})))

(defrecord INestedChecker
    [transform inverse-transform a b]
  p/Pattern
  (pattern-at [pattern point]
    (let [{:keys [a b]} pattern
          x (int (Math/floor (tup/x point)))
          y (int (Math/floor (tup/y point)))
          z (int (Math/floor (tup/z point)))]
      (if (zero? (mod (+ x y z) 2))
        (p/pattern-at a (matrix/mat-mul-tup (:inverse-transform a) point))
        (p/pattern-at b (matrix/mat-mul-tup (:inverse-transform b) point))))))

(defn nested-checker-pattern [pattern-a pattern-b]
  (map->INestedChecker
   (merge pattern-data {:a pattern-a :b pattern-b})))

(defn pattern-at-shape [pattern shape point]
  (let [obj-point (matrix/mat-mul-tup (:inverse-transform shape) point)
        pat-point (matrix/mat-mul-tup (:inverse-transform pattern) obj-point)]
    (p/pattern-at pattern pat-point)))

(defn chevron-pattern [color-a color-b]
  (let [half-scale (xform/scaling 0.50 0.50 0.50)
        horizontal (xform/rotation-y (/ Math/PI 2))
        pattern (stripe-pattern color-a color-b)]
    (xform/apply-transform
     (nested-checker-pattern
      (xform/apply-transform pattern half-scale horizontal)
      (xform/apply-transform pattern half-scale))
     (xform/rotation-y (/ Math/PI -4)))))
