(ns lapa3.core
  (:gen-class)
  (:require
   [clojure.string :as str]
   [clojure.tools.cli :refer [parse-opts]]))

(def window-sizes
  {:linear 2
   :lagrange 5})

(def interpolation-types
  {:linear "Линейная интерполяция"
   :lagrange "Лагранжевская интерполяция"
   :all "Обе интерполяции"})

(defn validate-types [type]
  (contains? interpolation-types (keyword type)))

(def cli-options
  [["-a"  "--algorithm NAME" "Имя типа интерполяции (linear, lagrange, all)"
    :id :algorithm
    :parse-fn #(str/lower-case (str/trim %))
    :validate [validate-types  "Должен быть 'linear', 'lagrange' или 'all'"]]

   ["-s"  "--step NUMBER" "Размер шага для интерполяции"
    :id :step
    :default 1.0
    :parse-fn #(Double/parseDouble %)]
   ["-H"  "--HELP" "Показать справочку"
    :id :help]])

(defn exit-program
  ([code]
   (System/exit code))
  ([code message]
   (println message)
   (System/exit code)))

(defn parse-line [line]
  (try
    (let [[x y] (str/split line #"[;\t\s]+")]
      [(Double/parseDouble x) (Double/parseDouble y)])
    (catch Exception e
      (println "Ошибка при парсинге строки. Завершаем программу.")
      (exit-program 1 e))))

(defn my-linear-interpolation [points step]
  (let [[p1 p2] points
        [x1 y1] p1
        [x2 y2] p2
        x-values (range x1 (+ step x2) step)]
    (map (fn [x]
           (let [t (/ (- x x1) (- x2 x1))]
             [(double x) (+ y1 (* t (- y2 y1)))]))
         x-values)))

(defn lagrange-polynomial [points x]
  (let [n (count points)]
    (reduce
     (fn [acc i]
       (let [[xi yi] (nth points i)
             li (reduce (fn [li j]
                          (if (not= i j)
                            (let [[xj _] (nth points j)]
                              (* li (/ (- x xj) (- xi xj))))
                            li))
                        1
                        (range n))]
         (+ acc (* li yi))))
     0
     (range n))))

(defn my-lagrange-interpolation [points step start-x end-x]
  (let [x-values (range start-x (+ end-x step) step)]
    (map (fn [x] [x (lagrange-polynomial points x)]) x-values)))

(defn format-output [x-values y-values]
  (let [x-str (str/join "\t" (map #(format "%.2f" %) x-values))
        y-str (str/join "\t" (map #(format "%.2f" %) y-values))]
    (str x-str "\n" y-str "\n")))

(defn format-interpolation-output [x-values y-values start-x end-x interpolation-type]
  (println (format "%s (X от %.3f до %.3f):"
                   interpolation-type start-x end-x))
  (println (format-output x-values y-values)))

(defn sorted-points? [points]
  (apply <= (map first points)))

(defn get-interpolation [interpolation-type window step]
  (cond
    (= interpolation-type "linear")
    (my-linear-interpolation window step)

    (= interpolation-type "lagrange")
    (let [start-x (first (map first window))
          end-x (last (map first window))]
      (my-lagrange-interpolation window step start-x end-x))))

(defn interpolate-and-print [interpolation-type window step]
  (let [interp (get-interpolation interpolation-type window step)
        x-values (map first interp)
        y-values (map second interp)]
    (format-interpolation-output x-values y-values
                                 (first x-values) (last x-values)
                                 (interpolation-types (keyword interpolation-type)))))

(defn process-input [step algorithm]
  (loop [points []]
    (let [line (read-line)]
      (when (nil? line)
        (exit-program 0 "EOF. ДО свидания."))
      (if (seq line)
        (let [point (parse-line line)
              updated-points (conj points point)
              sorted-points (if (not (sorted-points? updated-points))
                              (do
                                (println "Нет сортировки по Х. СОртировка...")
                                (sort-by first updated-points))
                              updated-points)]

          (when (>= (count sorted-points) 2)
            (when (or (= algorithm "linear") (= algorithm "all"))
              (interpolate-and-print "linear" (take-last 2 sorted-points) step)))

          (doseq [window (partition (window-sizes :lagrange) 1 sorted-points)]
            (interpolate-and-print "lagrange" window step))

          (recur sorted-points))
        (recur points)))))

(defn handle-args [args]
  (let [{:keys [options errors]} (parse-opts args cli-options)]
    (cond
      (:help options) {:action :help}
      errors          {:action :error, :errors errors}
      :else           {:action :run, :options options})))

(defn print-help []
  (println "Usage: lein run --step <step> --algorithm <algorithm>")
  (println "Опции:")
  (doseq [[short long desc & {:keys [default]}] cli-options]
    (println (str short ", " long "  " desc (when default (str " (по умолчанию: " default ")"))))))

(defn print-errors [errors]
  (println "Ошибки:")
  (doseq [error errors]
    (println error))
  (exit-program 1 "Завершение работы."))

(defn run-app [options]
  (let [step (or (:step options) 1.0)
        algorithm (or (:algorithm options) "all")]
    (println "Введите точки в формате: X Y")
    (process-input step algorithm)))

(defn -main [& args]
  (let [{:keys [action options errors]} (handle-args args)]
    (case action
      :help  (print-help)
      :error (print-errors errors)
      :run   (run-app options))))
