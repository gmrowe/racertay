(ns racertay.camera
  (:require [racertay.matrix :as mat]
            [racertay.tuple :as tup]
            [racertay.ray :as ray]
            [racertay.canvas :as can]
            [racertay.world :as wor]))

(defn- half-width-and-height [hsize vsize field-of-view]
  (let [half-view (Math/tan (/ field-of-view 2))
        aspect-ratio (double (/ hsize vsize))]
     (if (>= aspect-ratio 1)
       [half-view (/ half-view aspect-ratio)]
       [(* half-view aspect-ratio) half-view])))

(defn camera [hsize vsize field-of-view]
  (let [[half-width half-height]
        (half-width-and-height hsize vsize field-of-view)]
    #:camera{:hsize hsize
             :vsize vsize
             :field-of-view field-of-view
             :transform mat/identity-matrix
             :inverse-transform mat/identity-matrix
             :half-width half-width
             :half-height half-height
             :pixel-size (/ (* half-width 2) hsize)}))

(defn apply-transform
  ([camera xform]
   (let [updated (update camera :camera/transform (partial mat/mat-mul xform))]
     (assoc updated
            :camera/inverse-transform
            (mat/inverse (:camera/transform updated)))))

  ([camera xform & more]
   (reduce apply-transform camera (cons xform more))))

(defn ray-for-pixel [camera px py]
  (let [{:camera/keys
         [pixel-size half-width half-height transform inverse-transform]} camera
        x-offset (* pixel-size (+ px 0.5))
        y-offset (* pixel-size (+ py 0.5))
        world-x (- half-width x-offset)
        world-y (- half-height y-offset)
        pixel (mat/mat-mul-tup
               inverse-transform (tup/point world-x world-y -1))
        origin (mat/mat-mul-tup inverse-transform (tup/point 0 0 0))
        direction (tup/normalize (tup/tup-sub pixel origin))]
    (ray/ray origin direction)))

(defn render [camera world]
  (let [{:camera/keys [hsize vsize]} camera
        image (can/canvas hsize vsize)
        coords (for [y (range 0 vsize), x (range 0 hsize)] [x y])]
    (reduce (fn [im [x y]]
              (let [ray (ray-for-pixel camera x y)
                    color (wor/color-at world ray)]
                (can/write-pixel im x y color)))
            image
            coords)))
