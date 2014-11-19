;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.
(require '[clojure.string :as str])

;;Load file
(def fileContent (slurp "C:/butterfly.txt"))
(def lines (str/split fileContent #"\r\n"))

(def removieSpace (fn[el](str/replace el " ", "")))
(def getSubvector (fn[el](str/split (removieSpace el) #",")))
(def removieLastEl (fn[el](take (- (count el) 1) el)))
;;(for [d lines] (getSubvector d ))

(def sourceData (map removieLastEl (map getSubvector lines)))



