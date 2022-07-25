(ns racertay.material
  (:require [racertay.color :as color]
            [racertay.fcmp :as fcmp]
            [racertay.light :as light]
            [racertay.tuple :as tup]))

(defn material []
  {:color (color/color 1 1 1)
   :ambient 0.1
   :diffuse 0.9
   :specular 0.9
   :shininess 200.0})

(defn color [material]
  (:color material))

(defn assoc-color [material new-color]
  (assoc material :color new-color))

(defn ambient [material]
  (:ambient material))

(defn assoc-ambient [material new-ambient]
  (assoc material :ambient new-ambient))

(defn diffuse [material]
  (:diffuse material))

(defn assoc-diffuse [material new-diffuse]
  (assoc material :diffuse new-diffuse))

(defn specular [material]
  (:specular material))

(defn assoc-specular [material new-specular]
  (assoc material :specular new-specular))

(defn shininess [material]
  (:shininess material))

(defn assoc-shininess [material new-shininess]
  (assoc material :shininess new-shininess))

(defn material-eq? [mat1 mat2]
  (and
   (color/color-eq? (color mat1) (color mat2))
   (fcmp/nearly-eq? (ambient mat1) (ambient mat2))
   (fcmp/nearly-eq? (diffuse mat1) (diffuse mat2))
   (fcmp/nearly-eq? (specular mat1) (specular mat2))
   (fcmp/nearly-eq? (shininess mat1) (shininess mat2))))

(defn- calc-ambient [effective-color material]
  (color/color-mul-scalar effective-color (ambient material)))

(defn- calc-diffuse [light-dot-normal effective-color material]
  (if (neg? light-dot-normal)
    (color/color 0 0 0)
    (color/color-mul-scalar
     effective-color (* (diffuse material) light-dot-normal))))

(defn- calc-specular [light-dot-normal normalv lightv eyev light material]
  (if (neg? light-dot-normal)
    (color/color 0 0 0)
    (let [reflectv (tup/reflect (tup/tup-neg lightv) normalv)
          reflect-dot-eye (tup/dot reflectv eyev)]
      (if (pos? reflect-dot-eye)
        (color/color-mul-scalar
         (light/intensity light)
         (* (specular material)
            (Math/pow reflect-dot-eye (shininess material))))
        (color/color 0 0 0)))))

(defn lighting [material light point eyev normalv]
  (let [effective-color (color/color-mul (color material) (light/intensity light))
        lightv (tup/normalize (tup/tup-sub (light/position light) point))
        light-dot-normal (tup/dot lightv normalv)]
    (color/color-add
     (calc-ambient effective-color material)
     (calc-diffuse light-dot-normal effective-color material)
     (calc-specular light-dot-normal normalv lightv eyev light material))))
