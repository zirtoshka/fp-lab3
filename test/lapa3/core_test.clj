(ns lapa3.core-test
  (:require [clojure.test :refer :all]
            [lapa3.core :refer :all]))

(deftest test-linear-interpolation
  (testing "Линейная интерполяция для двух точек"
    (let [points [[0 0] [2 4]]
          step 1
          result (my-linear-interpolation points step)
          expected '([0.0 0] [1.0 2N] [2.0 4])]
      (is (= result expected)))))

(deftest test-lagrange-interpolation
  (testing "Лагранжевская интерполяция для нескольких точек"
    (let [points [[0 0] [1 1] [2 4]]
          step 1.0
          start-x 0
          end-x 2
          result (my-lagrange-interpolation points step start-x end-x)
          expected '([0 0] [1.0 1.0] [2.0 4.0])]
      (is (= result expected)))))
