(ns racertay.canvas-test
  (:require [clojure.test :refer :all]
            [clojure.string :as s]
            [racertay.canvas :refer :all]
            [racertay.color :refer [color color-eq?]]))

(deftest canvas-creation-test
  (testing "a canvas has a width and a height"
    (let [c (canvas 10 20)]
      (is (= 10 (:width c)))
      (is (= 20 (:height c)))))

  (testing "all pixels of a canvas are initially black"
    (let [c (canvas 10 20)
          pixels (:pixels c)
          black (color 0 0 0)]
      (is (= 200 (count pixels)))
      (is (every? #(color-eq? black %) pixels))))

  (testing "a canvas can be initialized with an arbitrary color"
    (let [cyan (color 0 1 1)
          c (canvas 10 20 cyan)
          pixels (:pixels c)]
      (is (every? #(color-eq? cyan %) pixels)))))

(deftest canvas-writing-test
  (testing "pixels of a canvas can be written to"
    (let [init-canvas (canvas 10 20)
          red (color 1 0 0)
          c (write-pixel init-canvas 2 3 red)]
      (is (color-eq? red (pixel-at c 2 3))))))

(deftest canvas-to-ppm-test
  (testing "canvas-to-ppm ouputs a proper header"
    (let [c (canvas 5 3)
          ppm (canvas-to-ppm c)
          header (take 3 (s/split-lines ppm))]
      (is (= ["P3" "5 3" "255"] header))))

  (testing "canvas-to-ppm outputs proper pixel data"
    (let [c1 (color 1.5 0 0)
          c2 (color 0 0.5 0)
          c3 (color -0.5 0 1)
          c (-> (canvas 5 3)
                (write-pixel 0 0 c1)
                (write-pixel 2 1 c2)
                (write-pixel 4 2 c3))
          ppm (canvas-to-ppm c)
          pixel-data (->> (s/split-lines ppm)
                          (drop 3) ; drop the header
                          (take 3))]
      (is (= ["255 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
              "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0"
              "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"]
             pixel-data))))

  (testing "long lines in ppm file should be split"
    (let [c (canvas 10 2 (color 1.0 0.8 0.6))
          ppm (canvas-to-ppm c)
          pixel-data (->> (s/split-lines ppm)
                          (drop 3)    ; drop the header
                          (take 4))]
      (is (= ["255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
              "153 255 204 153 255 204 153 255 204 153 255 204 153"
              "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
              "153 255 204 153 255 204 153 255 204 153 255 204 153"]
             pixel-data))))

  (testing "ppm file must end in newline character"
    (let [c (canvas 5 3)
          ppm (canvas-to-ppm c)]
      (is (= \newline (last ppm))))))

