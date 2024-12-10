(ns lapa3.io 
  (:require
    [clojure.string :as str]))


(defn validate-point [point]
  (if (= 2 (count point))
    point
    (throw (ex-info "Invalid point format" {:point point}))))

(defn parse-point [line]
  (map #(Double/parseDouble %) (str/split line #"\s+")))

;; (parse-point "1 2")

(defn parse-input []
  (let [input (loop [points []]
                (let [line (str/trim (read-line))] 
                  (if (empty? line) 
                    points
                    (let [point (try
                                  (parse-point line)
                                  (catch Exception e
                                    (do
                                      (println "Ошибка при разборе точки. Пожалуйста, введите числа.")
                                      nil)))]
                      (if point
                        (recur (conj points point))
                        (recur points))))))]
    (if (empty? input)
      (do
        (println "Ошибка: не было введено ни одной точки. Пожалуйста, введите точки в формате: X Y.")
        (System/exit 1)) 
      input)))

 (map #(Double/parseDouble %) (str/split "1 2" #"\s+"))

(defn print-points [points]
  (doseq [point points]
    (println (str/join "\t" (map #(format "%.2f" %) point)))))