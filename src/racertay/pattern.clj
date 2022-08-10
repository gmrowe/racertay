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
   (merge p/pattern-data
          {:a color-a
           :b color-b})))
