(ns lapa3.interpol)

(defn linear-interpolation [x0 y0 x1 y1 step]
  (let [slope (/ (- y1 y0) (- x1 x0))]
    (map (fn [x] [x (+ y0 (* slope (- x x0)))])
         (range x0 x1 step))))


(linear-interpolation 1 2 3 6 0.5)


(defn lagrange-interpolation [points step]
  (let [x-values (map first points)
        y-values (map second points)
        lagrange-poly (fn [x]
                        (reduce +
                                (map-indexed (fn [i yi]
                                               (* yi 
                                                  (reduce * 
                                                          (for [j (range (count x-values))
                                                                :when (not= i j)]
                                                            (/ (- x (nth x-values j))
                                                               (- (nth x-values i)
                                                                  (nth x-values j)))))))
                                             y-values)))]
    (map (fn [x] [x (lagrange-poly x)])
         (range (apply min x-values) (apply max x-values) step)))
  )
(lagrange-interpolation [[0.1 1.25] [0.2 2.38] [0.3 3.79] [0.4 5.44] [0.5 7.14] ] 0.05)