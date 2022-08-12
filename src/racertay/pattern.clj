(ns racertay.pattern
  (:require [racertay.color :as color]
            [racertay.tuple :as tup]
            [racertay.matrix :as matrix]
            [racertay.protocols :as p]))

(defrecord IStripePattern
    [transform inverse-transform a b]
  p/Pattern
  (pattern-at [pattern point]
    (if (even? (int (Math/floor (tup/x point))))
      (:a pattern)
      (:b pattern))))

(defn stripe-pattern [color-a color-b]
  (map->IStripePattern
   (merge p/pattern-data {:a color-a :b color-b})))

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
   (merge p/pattern-data {:a color-a :b color-b})))


(defrecord IRingPattern
    [transform inverse-transform a b]
  p/Pattern
  (pattern-at [pattern point]
    (let [px (tup/x point)
          pz (tup/z point)
          radicand (+ (* px px) (* pz pz))]
      (if (zero? (mod (Math/floor (Math/sqrt radicand)) 2))
        (:a pattern)
        (:b pattern)))))

(defn ring-pattern [color-a color-b]
  (map->IRingPattern
   (merge p/pattern-data {:a color-a :b color-b})))
