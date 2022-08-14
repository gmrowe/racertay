(ns racertay.material
  (:require [racertay.color :as color]
            [racertay.fcmp :as fcmp]
            [racertay.light :as light]
            [racertay.tuple :as tup]
            [racertay.pattern :as patt]
            [racertay.protocols :as p]))

(def new-material
  #:material{:color color/white
             :ambient 0.1
             :diffuse 0.9
             :specular 0.9
             :shininess 200.0
             :reflective 0.0})

(defn assoc-shininess [material new-shininess]
  (assoc material :shininess new-shininess))

(defn material-eq?
  [mat1 mat2]
  (and
   (color/color-eq? (:material/color mat1) (:material/color mat2))
   (fcmp/nearly-eq? (:material/ambient mat1) (:material/ambient mat2))
   (fcmp/nearly-eq? (:material/diffuse mat1) (:material/diffuse mat2))
   (fcmp/nearly-eq? (:material/specular mat1) (:material/specular mat2))
   (fcmp/nearly-eq? (:material/shininess mat1) (:material/shininess mat2))))

(defn- calc-ambient [effective-color ambient]
  (color/color-mul-scalar effective-color ambient))

(defn- calc-diffuse
  [light-dot-normal effective-color diffuse in-shadow?]
  (if (or in-shadow? (neg? light-dot-normal))
    color/black
    (color/color-mul-scalar
     effective-color (* diffuse light-dot-normal))))

(defn- calc-specular
  [light-dot-normal normalv lightv eyev light specular shininess in-shadow?]
  (if (or in-shadow? (neg? light-dot-normal))
    color/black
    (let [reflectv (tup/reflect (tup/tup-neg lightv) normalv)
          reflect-dot-eye (tup/dot reflectv eyev)]
      (if (pos? reflect-dot-eye)
        (color/color-mul-scalar
         (:light/intensity light)
         (* specular (Math/pow reflect-dot-eye shininess)))
        color/black))))

(defn lighting
  [material object light point eyev normalv in-shadow?]
  (let [{:material/keys [color diffuse specular shininess ambient pattern]} material
        surface-color (if pattern (p/pattern-at-shape pattern object point) color)
        effective-color (color/color-mul surface-color (:light/intensity light))
        lightv (tup/normalize (tup/tup-sub (:light/position light) point))
        light-dot-normal (tup/dot lightv normalv)
        ambient (calc-ambient effective-color ambient)
        diffuse (calc-diffuse light-dot-normal effective-color diffuse in-shadow?)
        specular
        (calc-specular
         light-dot-normal normalv lightv eyev light specular shininess in-shadow?)]
    (color/color-add ambient diffuse specular)))
