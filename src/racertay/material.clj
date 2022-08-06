(ns racertay.material
  (:require [racertay.color :as color]
            [racertay.fcmp :as fcmp]
            [racertay.light :as light]
            [racertay.tuple :as tup]))

(def new-material
  #:material{:color (color/color 1 1 1)
             :ambient 0.1
             :diffuse 0.9
             :specular 0.9
             :shininess 200.0})

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
  [light-dot-normal effective-color diffuse in-shadow]
  (if (or in-shadow (neg? light-dot-normal))
    (color/color 0 0 0)
    (color/color-mul-scalar
     effective-color (* diffuse light-dot-normal))))

(defn- calc-specular
  [light-dot-normal normalv lightv eyev light specular shininess in-shadow]
  (if (or in-shadow (neg? light-dot-normal))
    (color/color 0 0 0)
    (let [reflectv (tup/reflect (tup/tup-neg lightv) normalv)
          reflect-dot-eye (tup/dot reflectv eyev)]
      (if (pos? reflect-dot-eye)
        (color/color-mul-scalar
         (:light/intensity light)
         (* specular (Math/pow reflect-dot-eye shininess)))
        (color/color 0 0 0)))))

(defn lighting
  [material light point eyev normalv in-shadow]
  (let [{:material/keys [color diffuse specular shininess ambient]} material
        effective-color (color/color-mul color (:light/intensity light))
        lightv (tup/normalize (tup/tup-sub (:light/position light) point))
        light-dot-normal (tup/dot lightv normalv)]
    (color/color-add
     (calc-ambient effective-color ambient)
     (calc-diffuse
      light-dot-normal effective-color diffuse in-shadow)
     (calc-specular
      light-dot-normal normalv lightv eyev light specular shininess in-shadow))))
