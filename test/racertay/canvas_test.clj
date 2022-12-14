(ns racertay.canvas-test
  (:require [clojure.test :refer :all]
            [clojure.string :as s]
            [racertay.canvas :refer :all]
            [racertay.color :as color]))

(deftest canvas-creation-test
  (testing "a canvas has a width and a height"
    (let [c (canvas 10 20)]
      (is (= 10 (:canvas/width c)))
      (is (= 20 (:canvas/height c)))))

  (testing "all pixels of a canvas are initially black"
    (let [c (canvas 10 20)
          pixels (:canvas/pixels c)]
      (is (= 200 (count pixels)))
      (is (every? #(color/color-eq? color/black %) pixels))))

  (testing "a canvas can be initialized with an arbitrary color"
    (let [cyan (color/color 0 1 1)
          c (canvas 10 20 cyan)
          pixels (:canvas/pixels c)]
      (is (every? #(color/color-eq? cyan %) pixels)))))

(deftest canvas-writing-test
  (testing "pixels of a canvas can be written to"
    (let [init-canvas (canvas 10 20)
          red (color/color 1 0 0)
          c (write-pixel init-canvas 2 3 red)]
      (is (color/color-eq? red (pixel-at c 2 3))))))

(deftest canvas-to-ppm-test
  (let [header-line-count 3]
    (testing "canvas-to-ppm ouputs a proper header"
      (let [c (canvas 5 3)
            ppm (canvas-to-ppm c)
            header (take header-line-count (s/split-lines ppm))]
        (is (= ["P3" "5 3" "255"] header))))

    (testing "canvas-to-ppm outputs proper pixel data"
      (let [c1 (color/color 1.5 0 0)
            c2 (color/color 0 0.5 0)
            c3 (color/color -0.5 0 1)
            c (-> (canvas 5 3)
                  (write-pixel 0 0 c1)
                  (write-pixel 2 1 c2)
                  (write-pixel 4 2 c3))
            ppm (canvas-to-ppm c)
            pixel-data (->> (s/split-lines ppm)
                            (drop header-line-count) ; drop the header
                            (take 3))]
        (is (= ["255 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
                "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0"
                "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255"]
               pixel-data))))

    (testing "long lines in ppm file should be split"
      (let [c (canvas 10 2 (color/color 1.0 0.8 0.6))
            ppm (canvas-to-ppm c)
            pixel-data (->> (s/split-lines ppm)
                            (drop header-line-count)    ; drop the header
                            (take 4))]
        (is (= ["255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
                "153 255 204 153 255 204 153 255 204 153 255 204 153"
                "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
                "153 255 204 153 255 204 153 255 204 153 255 204 153"]
               pixel-data)))))

  (testing "ppm file must end in newline character"
    (let [c (canvas 5 3)
          ppm (canvas-to-ppm c)]
      (is (= \newline (last ppm))))))

(deftest canvas-to-p6-ppm-test
  (testing "canvas-to-p6-ppm outputs correct header"
    (let [c (canvas 3 3)
          expected-header (vec (.getBytes (format "P6%n3 3%n255%n")))]
      (is (= expected-header
             (take (count expected-header) (canvas-to-p6-ppm c))))))

  (testing "canvas-to-p6-ppm outputs correct pixel data"
    (let [c (canvas 2 1 (color/color 1.5 0.5 -0.1))
          bytes-per-pixel 3
          pixel-byte-count (* bytes-per-pixel (count (:canvas/pixels c)))
          ppm (vec (canvas-to-p6-ppm c))
          pixel-data (take-last pixel-byte-count ppm)]
      (is (= [-1 -128 0 -1 -128 0] pixel-data)))))
