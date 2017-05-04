(ns omnom.macro)

(defmacro project-version
  []
  (-> "project.clj" slurp read-string (nth 2)))
