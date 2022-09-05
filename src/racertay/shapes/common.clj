(ns racertay.shapes.common
  (:require [racertay.matrix :as matrix]
            [racertay.material :as material]))

(defn shape-data []
  {:id (java.util.UUID/randomUUID)
   :transform matrix/identity-matrix
   :inverse-transform matrix/identity-matrix
   :material material/default-material})
