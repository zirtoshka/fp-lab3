(ns lapa3.core
  (:gen-class)
  (:require
   [clojure.string :as str]
   [clojure.tools.cli :refer [parse-opts]]
   [lapa3.interpol :refer [lagrange-interpolation linear-interpolation
                           sliding-window]]
   [lapa3.io :refer [parse-input print-points]]))


(def window-sizes
  {:linear 2
   :lagrange 5})


(def interpolation-types
  {"linear" "Линейная интерполяция"
   "lagrange" "Лагранжевская интерполяция"
   "all" "Обе интерполяции"})


(defn validate-types [type]
  (contains? interpolation-types type))

(def cli-options
  [["-t" "--type NAME" "Имя типа интерполяции (linear, lagrange, all)"
    :id :interpol-type
    :update-fn conj
    :multi true ;;ex: -a linear -a lagrange.
    :default []
    :parse-fn #(str/lower-case (str/trim %))
    :validate [validate-types  "Должен быть 'linear', 'lagrange' или 'all'"]]
   ["-s" "--step NUMBER" "Размер шага для интерполяции"
    :id :step
    :default 1.0
    :parse-fn #(Double/parseDouble %)]
   ["-H" "--HELP"
    :id :help]])

(defn error-msg [errors]
  (if (seq errors)
    (str "Возникли следующие ошибки:\n"
         (str/join \newline errors))
    "Ошибок нет."))

(defn exit [status msg]
  (let [output (if (zero? status) System/out System/err)]
    (.println output msg)
    (System/exit status)))

;; (exit 0 "Program finished successfully.")


(defn parse-args [args]
  (let [{:keys [options summary]} (parse-opts args cli-options)]
    (cond
      (:help options) (exit 0 (str "Usage:\n" summary))
      (empty? (:interpol-type options)) (exit 1 "Не указан тип интреполяции")
      :else options)))


(defn process-input [step interpol-types]
  (println step " -step- ")
  (let [input-data (parse-input)]
    (if (empty? input-data)
      (println "Введите точки в формате: X Y")
      ((println input-data)
       (println interpol-types "- types")
       (doseq [interpol-type interpol-types]
         (println interpol-type)
         (let [window-size (window-sizes (keyword interpol-type))]
           (println (str "Используется: " interpol-type))
           (doseq [window (sliding-window input-data window-size)]
             (let [result (case interpol-type
                            "linear" (linear-interpolation (first window) (second window) step)
                            "lagrange" (lagrange-interpolation window step)
                            (throw (ex-info "Неизвестный тип интерполяции" {:type interpol-type})))]
               (print-points result)))))))))


(defn -main [& args]
  (println args "-args")
  (let [options (parse-args args)]
    (process-input (:step options) (:interpol-type options))))


;; (defn -main [& args]
;;   (let [options (parse-args args)
;;         input-data (parse-input)]
;;     (if (empty? input-data)
;;       (println "Введите точки в формате: X Y")
;;       (do
;;         (if (contains? (set (:interpol-type options)) "linear")
;;           (linear-interpolation input-data (:step options)))
;;         (if (contains? (set (:interpol-type options)) "lagrange")
;;           (lagrange-interpolation input-data))))))



;; (defn -main [& args]
;;   (let [options (parse-args args)]

;;     (println "Parsed options:" options)
;;     ((println "Run the program with: lein run --step <step> --type <interpolation-name>")
;;      (println "Performing calculations with options:" options))))

;; (+ 1 2)
;; (parse-opts ["-t" "linear"] cli-options)
;; (+ 4 2)





