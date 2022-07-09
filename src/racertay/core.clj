(ns racertay.core
  (:require [racertay.tuple :refer :all]
            [racertay.fcmp :refer :all])
  (:gen-class))

(defn projectile [position velocity]
  {:position position
   :velocity velocity})

(defn environment [gravity wind]
  {:gravity gravity
   :wind wind})

(defn tick [env proj]
  (let [new-pos (tup-add (:position proj) (:velocity proj))
        new-vel (tup-add (tup-add (:velocity proj) (:gravity env))
                         (:wind env))]
    (projectile new-pos new-vel)))

(defn flight-path [env proj]
  (map :position (iterate (partial tick env) proj)))

(defn -main
  [& args]
  (let [p (projectile (point 0 1 0) (normalize (vect 1 1 0)))
        e (environment (vect 0 -0.1 0) (vect -0.01 0 0))
        flight (take-while  #(> (:y %) 0) (flight-path e p))]
    (println flight)))
