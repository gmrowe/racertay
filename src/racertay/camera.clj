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

(defn update-progress [total]
  (fn [curr]
    (let [progress (double (* 100 (/ curr total)))]
      (do
        (print
         (str (format "%.0f%% complete" progress) \return))
        (flush)))))

(defn seq-peek
  "Calls callback after n entries in s are evaluated."
  [n callback s]
  (let [call-and-return
       (fn [iter data]
         (do (if (zero? (rem iter n))
               (callback iter))
             data))]
    (map call-and-return
         (iterate inc 1) s)))

(defn report-progress [xs]
  (seq-peek
   (max 1 (/ (count xs) 100)) (update-progress (count xs)) xs))

(defn write-image-pixel [camera world canvas [x y]]
  (let [ray (ray-for-pixel camera x y)
        color (wor/color-at world ray)]
    (can/write-pixel canvas x y color)))

(defn render
  ([camera world] (render camera world nil))
  ([camera world report?]
   (let [{:camera/keys [hsize vsize]} camera
         coords (for [y (range 0 vsize), x (range 0 hsize)] [x y])
         action (if report? report-progress identity)]
     (reduce (partial write-image-pixel camera world)
             (can/canvas hsize vsize)
             (action coords)))))
