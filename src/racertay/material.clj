(ns racertay.material
  (:require [racertay.color :as color]
            [racertay.fcmp :as fcmp]))

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
