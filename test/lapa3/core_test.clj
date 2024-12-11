(ns lapa3.core-test
  (:require [clojure.test :as an-alias]
            [lapa3.core :refer [my-lagrange-interpolation my-linear-interpolation]]))

(an-alias/deftest test-linear-interpolation
  (an-alias/testing "Линейная интерполяция для двух точек"
    (let [points [[0 0] [2 4]]
          step 1
          result (my-linear-interpolation points step)
          expected '([0.0 0] [1.0 2N] [2.0 4])]
      (an-alias/is (= result expected)))))

(an-alias/deftest test-lagrange-interpolation
  (an-alias/testing "Лагранжевская интерполяция для нескольких точек"
    (let [points [[0 0] [1 1] [2 4]]
          step 1.0
          start-x 0
          end-x 2
          result (my-lagrange-interpolation points step start-x end-x)
          expected '([0 0] [1.0 1.0] [2.0 4.0])]
      (an-alias/is (= result expected)))))
