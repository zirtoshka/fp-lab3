(ns lapa3.interpol)

(defn linear-interpolation [[x0 y0] [x1 y1 ] step]
  (if (> x1 x0)
    (let [slope (/ (- y1 y0) (- x1 x0))]
      (map (fn [x] [x (+ y0 (* slope (- x x0)))])
           (range x0 (+ x1 step) step)))
    (throw (ex-info "x1 must be greater than x0" {:x0 x0 :x1 x1}))))


;; (linear-interpolation 1 2 3 6 0.5)


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
         (range (apply min x-values) (+ (apply max x-values) step) step))))
;; (lagrange-interpolation [[0.1 1.25] [0.2 2.38] [0.3 3.79] [0.4 5.44] [0.5 7.14] ] 0.05)

;; (lagrange-interpolation [[1 1] [2 4] [3 9]] 0.5)


(defn sliding-window [data window-sizes]
  (when (>= (count data) window-sizes)
    (lazy-seq
     (cons (take window-sizes data)
           (sliding-window (rest data) window-sizes)))))

;; (sliding-window [1 2 3 4] 3)