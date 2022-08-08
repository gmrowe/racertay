(ns racertay.pattern
  (:require [racertay.color :as color]
            [racertay.tuple :as tup]))

(defn stripe-pattern [color-a color-b]
  #:stripe-pattern{:a color-a
                   :b color-b})

(defn stripe-at [stripe-patt point]
  (if (even? (int (Math/floor (tup/x point))))
    (:stripe-pattern/a stripe-patt)
    (:stripe-pattern/b stripe-patt)))
