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
  (let [{:canvas/keys [width height]} canvas]
    (if (and (int-in-range? 0 width x)
             (int-in-range? 0 height y))
      (let [index (+ (* width y) x)]
        (assoc-in canvas [:canvas/pixels index] color))
      canvas)))

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
  (let [{:keys [red green blue]} pixel]
    (-> bs
        (conj (unchecked-byte (scale-subpixel red)))
        (conj (unchecked-byte (scale-subpixel green)))
        (conj (unchecked-byte (scale-subpixel blue))))))

(defn- output-pixel
  [pixel]
  (let [{:keys [red green blue]} pixel]
    (format "%s %s %s"
            (scale-subpixel red)
            (scale-subpixel green)
            (scale-subpixel blue))))

(defn- output-line [line]
  (s/join " " (map output-pixel line)))

(defn canvas-to-p6-ppm [canvas]
  (let [{:canvas/keys [width height pixels]} canvas
        header (format "P6\n%s %s\n%s\n" width height max-subpixel-value)]
    (byte-array
     (reduce conj-pixel-bytes (vec (.getBytes header)) pixels))))

(defn canvas-to-ppm [canvas]
  (let [{:canvas/keys [width height pixels]} canvas
        header (format "P3%n%s %s%n%s%n" width height max-subpixel-value)
        pixel-data (->> pixels
                        (partition width)
                        (map output-line)
                        (map (partial break-line max-ppm-line-len))
                        (s/join "\n"))]
    (str header pixel-data \newline)))
