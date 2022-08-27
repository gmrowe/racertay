(ns racertay.fcmp)

(def epsilon 0.00001)

(defn nearly-eq? [x y]
  (< (abs (- x y)) epsilon))

(defn nearly-zero? [x]
  (nearly-eq? x 0.0))
