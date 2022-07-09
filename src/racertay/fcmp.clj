(ns racertay.fcmp)

(def epsilon 0.00001)

(defn nearly-eq? [x y]
  (< (Math/abs (- x y)) epsilon))
