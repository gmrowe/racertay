(ns racertay.canvas
  (:require [clojure.string :as s]
            [racertay.color :refer [color]]))

(defn canvas
  ([w h] (canvas w h (color 0 0 0)))
  ([w h color]
   #:canvas{:width w
            :height h
            :pixels (vec (repeat (* w h) color))}))

(defn- int-in-range? [start end val]
  (and (<= start val) (< val end)))

(defn write-pixel [canvas x y color]
  (if (and (int-in-range? 0 (:canvas/width canvas) x)
           (int-in-range? 0 (:canvas/height canvas) y))
    (let [index (+ (* (:canvas/width canvas) y) x)]
      (assoc-in canvas [:canvas/pixels index] color))
    canvas))

(defn pixel-at [canvas x y]
  (let [index (+ (* (:canvas/width canvas) y) x)]
    (get-in canvas [:canvas/pixels index])))

(defn- clamp [value min max]
  (cond
    (< value min) min
    (> value max) max
    :else         value))

(def max-subpixel-value 255)
(def max-ppm-line-len 70)

(defn- split-str-around-index [str index]
  [(subs str 0 index)
   (subs str (inc index))])

(defn- break-line [max-line-len line]
  (if (< (count line) max-line-len)
    line
    (let [break-index (s/last-index-of line " " max-line-len)
          [begin more] (split-str-around-index line break-index)
          end (break-line max-line-len more)]
      (format "%s%n%s" begin end))))

(defn- scale-subpixel [sub]
  (-> (* max-subpixel-value sub)
      (Math/round)
      (clamp 0 max-subpixel-value)))

(defn- conj-pixel-bytes [bs pixel]
  (-> bs
      (conj (unchecked-byte (scale-subpixel (:red pixel))))
      (conj (unchecked-byte (scale-subpixel (:green pixel))))
      (conj (unchecked-byte (scale-subpixel (:blue pixel))))))

(defn- output-pixel
  [p]
  (format "%s %s %s"
          (scale-subpixel (:red p))
          (scale-subpixel (:green p))
          (scale-subpixel (:blue p))))

(defn- output-line [line]
  (s/join " " (map output-pixel line)))

(defn canvas-to-p6-ppm [c]
  (let [header (format "P6\n%s %s\n%s\n" (:canvas/width c) (:canvas/height c) max-subpixel-value)]
    (byte-array
     (reduce conj-pixel-bytes (vec (.getBytes header)) (:canvas/pixels c)))))

(defn canvas-to-ppm [c]
  (let [header (format "P3%n%s %s%n%s%n" (:canvas/width c) (:canvas/height c) max-subpixel-value)
        pixel-data (->> (:canvas/pixels c)
                        (partition (:canvas/width c))
                        (map output-line)
                        (map (partial break-line max-ppm-line-len))
                        (s/join "\n"))]
    (str header pixel-data \newline)))
