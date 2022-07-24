(ns racertay.material
  (:require [racertay.color :as color]))

(defn material []
  {:color (color/color 1 1 1)
   :ambient 0.1
   :diffuse 0.9
   :specular 0.9
   :shininess 200.0})

(defn color [material]
  (:color material))

(defn ambient [material]
  (:ambient material))

(defn diffuse [material]
  (:diffuse material))

(defn specular [material]
  (:specular material))

(defn shininess [material]
  (:shininess material))
