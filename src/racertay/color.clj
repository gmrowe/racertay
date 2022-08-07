(ns racertay.color
  (:require [racertay.fcmp :refer :all]))

(defn color [r g b]
  {:red (double r)
   :green (double g)
   :blue (double b)})

(def max-subpixel-value 255.0)

(defn hex->color [hex]
  (let [r (bit-and (bit-shift-right hex (* 8 2)) 0xFF)
        g (bit-and (bit-shift-right hex (* 8 1)) 0xFF)
        b (bit-and (bit-shift-right hex (* 8 0)) 0xFF)] 
    (color (/ r max-subpixel-value)
           (/ g max-subpixel-value)
           (/ b max-subpixel-value))))

;; Names and values taken from https://www.w3schools.com/tags/ref_colornames.asp
(def black (color 0 0 0))
(def white (color 1 1 1))
(def red (color 1 0 0))
(def lime (color 0 1 0))
(def blue (color 0 0 1))
(def cyan (color 0 1 1))
(def aqua cyan)
(def yellow (color 1 1 0))
(def magenta (color 1 0 1))
(def fuchsia magenta)
(def pink (hex->color 0xFFC0CB))
(def orange (hex->color 0xFFA500))
(def indigo (hex->color 0x4B0082))
(def purple (hex->color 0x800080))
(def violet (hex->color 0xEE82EE))
(def beige (hex->color 0xF5F5DC))
(def brown (hex->color 0xA52A2A))
(def chartreuse (hex->color 0x7FFF00))
(def dark-red (hex->color 0x8B0000))
(def dark-green (hex->color 0x006400))
(def dark-blue (hex->color 0x00008B))
(def gold (hex->color 0xFFD700))
(def gray (hex->color 0x808080))
(def grey gray)
(def green (hex->color 0x008000))
(def light-pink (hex->color 0XFFB6C1))
(def light-blue (hex->color 0xADD8E6))
(def light-green (hex->color 0x90EE90))
(def light-cyan (hex->color 0xE0FFFF))
(def navy (hex->color 0x000080))
(def tan (hex->color 0xD2B48C))

(defn color-eq? [c1 c2]
  (and
   (nearly-eq? (:red c1) (:red c2))
   (nearly-eq? (:green c1) (:green c2))
   (nearly-eq? (:blue c1) (:blue c2))))

(defn color-add
  ([c] c)
  ([c1 c2]
   (color (+ (:red c1) (:red c2))
          (+ (:green c1) (:green c2))
          (+ (:blue c1) (:blue c2))))
  ([c1 c2 & more]
   (reduce color-add c1 (cons c2 more))))

(defn color-sub
  ([c]
   (color-sub black c))
  
  ([c1 c2]
   (color (- (:red c1) (:red c2))
          (- (:green c1) (:green c2))
          (- (:blue c1) (:blue c2))))
  
  ([c1 c2 & more]
   (reduce color-sub c1 (cons c2 more))))

(defn color-mul-scalar [c scalar]
  (color (* (:red c) scalar)
         (* (:green c) scalar)
         (* (:blue c) scalar)))

(defn color-mul [c1 c2]
  (color (* (:red c1) (:red c2))
         (* (:green c1) (:green c2))
         (* (:blue c1) (:blue c2))))
