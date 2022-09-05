(ns racertay.core
  (:require [clojure.java.io :as io]
            [racertay.camera :as cam]
            [racertay.canvas :as canv]
            [racertay.scenes.cone :as scene])
  (:gen-class))

(defn write-canvas-to-ppm-file [canvas filename]
  (let [ppm-bytes (canv/canvas-to-p6-ppm canvas)]
    (with-open [out (io/output-stream filename)]
      (.write out ppm-bytes))))

(def width 800)
(def height 600)

(defn -main [& args]
  (let [{:keys [camera world]} (scene/scene width height)
        canvas (cam/render camera world :report)]
    (write-canvas-to-ppm-file canvas "cone.ppm")))
