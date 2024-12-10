(ns lapa3.core
  (:gen-class)
  (:require
   [clojure.string :as str]
   [clojure.tools.cli :refer [parse-opts]]))
  


(def interpolation-types
  {"linear" (+ 1 2)
   "lagrange" (+ 2 33)
   "all" (+ 3 4)})

(def window-sizes
  {:linear 2
   :lagrange 5})

(defn validate-types [type]
  (if (contains? interpolation-types type)
    true
    (str "Invalid interpolation type name: " type)))


(def cli-options
  [["-t" "--type NAME" "Interpolation type name"
    :id :interpol-type
    :multi true ;;ex: -a linear -a lagrange.
    :default []
    :parse-fn #(str/lower-case (str/trim %))
    :validate [validate-types "Must be one of: linear, lagrange"]]
   ["-s" "--step NUMBER" "Step size for point calculation"
    :id :step
    :default 1.0
    :parse-fn #(Double/parseDouble %)]
   ["-H" "--HELP"
    :id :help]])

(defn error-msg [errors]
  (if (seq errors)
    (str "The following errors occurred during the processing of your command:\n"
         (str/join \newline errors))
    "No errors occurred."))

(defn exit [status msg]
  (let [output (if (zero? status) System/out System/err)]
    (.println output msg)
    (System/exit status)))

;; (exit 0 "Program finished successfully.")


(defn parse-args [args]
  (let [{:keys [options summary]} (parse-opts args cli-options)]
    (cond
      (:help options) (exit 0 (str "Usage:\n" summary))
      (empty? (:interpol-type options)) (exit 1 "No interpolation type provided.")
      :else options)
    ))





(defn -main [& args]
  (let [options (parse-args args)]
    
    (println "Parsed options:" options)
    ((println "Run the program with: lein run --step <step> --type <interpolation-name>")
     (println "Performing calculations with options:" options))))

(+ 1 2)
(parse-opts ["-t" "linear"] cli-options)
(+ 4 2)



