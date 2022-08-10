(ns racertay.pattern
  (:require [racertay.color :as color]
            [racertay.tuple :as tup]
            [racertay.matrix :as matrix]))

(defn stripe-pattern [color-a color-b]
  #:stripe-pattern{:a color-a
                   :b color-b
                   :transform matrix/identity-matrix
                   :inverse-transform matrix/identity-matrix})

(defn apply-transform [stripe-patt xform]
  (let [updated (update stripe-patt
                        :stripe-pattern/transform
                        (partial matrix/mat-mul xform))]
    (assoc updated
           :stripe-pattern/inverse-transform
           (matrix/inverse (:stripe-pattern/transform updated)))))

(defn stripe-at [stripe-patt point]
  (if (even? (int (Math/floor (tup/x point))))
    (:stripe-pattern/a stripe-patt)
    (:stripe-pattern/b stripe-patt)))

(defn stripe-at-object [stripe-patt object point]
  (let [obj-point (matrix/mat-mul-tup (:inverse-transform object) point)
        pat-point (matrix/mat-mul-tup (matrix/inverse (:stripe-pattern/transform stripe-patt))
                                      obj-point)]
    (stripe-at stripe-patt pat-point)))
