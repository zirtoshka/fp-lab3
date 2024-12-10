(ns lapa3.core
  (:gen-class)
  (:require
   [clojure.string :as str]
   [clojure.tools.cli :refer [parse-opts]]))
  

(+ 1 2)

(def interpolation-types
  {"linear" (+ 1 2)
   "lagrange" (+ 2 33)})

(def window-sizes
  {:linear 2
   :lagrange 5})

(defn validate-types [type]
  (if (contains? interpolation-types type)
    true
    (str "Invalid interpolation type name: " type)))

(validate-types "linear")

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




(defn parse-line [line]
  (let [[x y] (str/split line #"\s+")]
    [(Double/parseDouble x) (Double/parseDouble y)]))

(defn read-input []
  (map parse-line (line-seq (java.io.BufferedReader. *in*))))

(defn linear-interpolate [points step]
  (let [pairs (partition 2 1 points)]
    (mapcat
     (fn [[[x1 y1] [x2 y2]]]
       (let [dx (- x2 x1)
             dy (- y2 y1)
             slope (/ dy dx)]
         (for [x (range x1 x2 step)]
           [x (+ y1 (* slope (- x x1)))])))
     pairs)))

