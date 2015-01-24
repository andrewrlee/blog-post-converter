(ns post-convert.core
 (:require [clojure.string :refer [split trim replace lower-case join] :rename {replace string-replace}])
 (:gen-class))

(def files  (filter (fn [a] (re-find #".*.txt" (.getName a))) (file-seq (java.io.File. "resources"))))

(defn extract-key-values-seq [file] 
  (filter not-empty 
    (map #(rest (re-matches #"(?s)(.*?)\n(.*)" %)) (split (slurp file) #"#")))) 

(defn keywordize [word] 
  (-> word
      (string-replace ":" "")
      (trim)
      (lower-case)
      (keyword)))   

(defn extract-map [text] 
  (apply merge 
    (map 
      (fn [[k v]] (hash-map (keywordize k) (trim v))) 
      (extract-key-values-seq text))))

(defn convert-date [date]
  (->> date
      (.parse  (java.text.SimpleDateFormat. "dd/MM/yyyy"))
      (.format (java.text.SimpleDateFormat. "yyyy-MM-dd"))))

(defn convert-to-dash-separated [text] 
  (join "-"
    (-> text
        (lower-case)
        (string-replace "." "")
        (split #"[\s]+"))))

(defn post-file-name [date post] 
  (let [title  (convert-to-dash-separated (:subject post ))] 
    (str date "-" title ".md" )))

(defn convert-to-yaml-array [string] 
  (str "[" (->  (or string "")
                (string-replace ";" ",")
                (string-replace #",$" ""))  
       "]"))

(defn -main
  [& args]
  (doseq [post (map extract-map files)
          :let [date               (convert-date (:date post))
                file-name          (post-file-name date post)
                front-matter-vals  {:layout   "post" 
                                    :title    (:subject post)  
                                    :category (:category post)
                                    :tags     (convert-to-yaml-array (:tags post))
                                    :date     date}
                front-matter (str "---\n" (apply str (map (fn [[k v]] (str (name k) ": " v "\n"))  (seq front-matter-vals))) "---")]]
    (println "Generating.. " file-name )
    (spit (str "output/" file-name) (str front-matter "\n\n" (:summary post) "\n\n" (:contents post)))))
