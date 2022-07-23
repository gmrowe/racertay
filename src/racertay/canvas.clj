(ns racertay.canvas
  (:require [clojure.string :as s]
            [racertay.tuple :refer :all]
            ;[racertay.fcmp :refer :all]
            [racertay.color :refer [color]]))


(defn canvas
  ([w h] (canvas w h (color 0 0 0)))
  ([w h color]
   {:width w
    :height h
    :pixels (vec (repeat (* w h) color))}))

(defn- int-in-range? [start end val]
  (and (<= start val) (< val end)))

(defn write-pixel [canvas x y color]
  (if (and (int-in-range? 0 (:width canvas) x)
           (int-in-range? 0 (:height canvas) y))
    (let [index (+ (* (:width canvas) y) x)]
      (assoc-in canvas [:pixels index] color))
    canvas))

(defn pixel-at [canvas x y]
  (let [index (+ (* (:width canvas) y) x)]
    (get-in canvas [:pixels index])))

(defn clamp [value min max]
  (cond
    (< value min) min
    (> value max) max
    :else         value))

(def max-subpixel-value 255)
(def max-ppm-line-len 70)

(defn- output-pixel
  [p]
  (letfn [(scale-subpixel [sub]
            (-> (* max-subpixel-value sub)
                (Math/round)
                (clamp 0 max-subpixel-value)))]
    (format "%s %s %s"
            (scale-subpixel (:red p))
            (scale-subpixel (:green p))
            (scale-subpixel (:blue p)))))

(defn- split-str-around-index [str index]
  [(subs str 0 index)
   (subs str (inc index))])

(defn- output-line [line]
  (s/join " " (map output-pixel line)))

(defn- break-line [max-line-len line]
  (if (< (count line) max-line-len)
    line
    (let [break-index (s/last-index-of line " " max-line-len)
          [begin more] (split-str-around-index line break-index)
          end (break-line max-line-len more)]
      (format "%s%n%s" begin end))))

(defn- subpixel->byte [sp]
  (-> sp
      (* max-subpixel-value)
      (int)
      (clamp 0 max-subpixel-value)
      (bit-and 0xFF)
      (unchecked-byte)))

(defn- conj-pixel [bs pixel]
  (-> bs
      (conj (subpixel->byte (:red pixel)))
      (conj (subpixel->byte (:green pixel)))
      (conj (subpixel->byte (:blue pixel)))))

(defn canvas-to-p6-ppm [c]
  (let [header (format "P6\n%s %s\n%s\n" (:width c) (:height c) max-subpixel-value)
        bs (reduce conj-pixel (vec (.getBytes header)) (:pixels c))]
    (byte-array bs)))

(defn canvas-to-ppm [c]
  (let [header (format "P3%n%s %s%n%s%n" (:width c) (:height c) max-subpixel-value)
        pixel-data (->> (:pixels c)
                        (partition (:width c))
                        (map output-line)
                        (map (partial break-line max-ppm-line-len))
                        (s/join "\n"))]
    (str header pixel-data \newline)))
