;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.
(require '[clojure.string :as str])


;;CONSTANTS
(def DistanceName "Eq")
(def Ra 3)
(def Rb (* 1.5 Ra))
(def EpsHigh 0.5)
(def EpsLow 0.15)

(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

;;Load file
(def fileContent (slurp "C:/butterfly.txt"))
(def lines (str/split fileContent #"\r\n"))

(def removieSpace (fn[el](str/replace el " ", "")))
(def getSubvector (fn[el](map parse-int (str/split (removieSpace el) #","))))
(def removieLastEl (fn[el](take (- (count el) 1) el)))


(def sourceData (map removieLastEl (map getSubvector lines)))


;;LOGIC

;;(defn euclidian-distance [c1 c2] (->> (map - c1 c2) (map #(* % %)) (reduce +)))

(defn euclidian-distance [c1 c2] (->> (map - c1 c2) (map #(* % %)) (reduce +) (Math/sqrt)))
(defn hamming-distance [c1 c2] (count (filter true? (map not= c1 c2))))


(def calculate-distance (if (= DistanceName "Hamming") hamming-distance euclidian-distance))

;;(hamming-distance (nth sourceData 0) (nth sourceData 1))

(defn create-point-with-dist
  [point dist]
  (assoc {:coordinates (into [] point)} :distance dist))



;;GET POTENCIALS
(defn potential
  [distance]
  (Math/exp (- (* (/ 4 (* Ra Ra)) distance))))

(defn revised-potential
  [distance]
  (Math/exp (- (* (/ 4 (* Rb Rb)) distance))))


(defn calc-potencial [item items]
  (potential (reduce + (map #(calculate-distance item %1) items)))
  )


(defn getPotentialsVector [elements]
  (map #(create-point-with-dist %1 (calc-potencial %1 elements)) elements))





(defn recalcCurrentPotencial [el, kernel] (
                                    assoc el :distance (- (:distance el) (* (:distance kernel) (revised-potential (calculate-distance (:coordinates el) (:coordinates kernel)))))
                                  ))


(defn recalculatePotencials [potencoals kernel]
  (map #(recalcCurrentPotencial %1 kernel) potencoals)
  )



(def kern {:distance 2.9374821117108028E-30, :coordinates [5 3]})
(def els
[{:distance 2.4408357381215022E-83, :coordinates [8 1]} {:distance 7.276028788610185E-80, :coordinates [1 0]} {:distance 7.276028788610185E-80, :coordinates [7 6]} {:distance 1.2833033941286201E-75, :coordinates [0 3]} {:distance 2.5596432601257677E-67, :coordinates [1 5]} {:distance 1.1958644107155672E-52, :coordinates [7 3]} {:distance 6.529198451554616E-51, :coordinates [2 1]} {:distance 6.529198451554616E-51, :coordinates [6 5]} {:distance 2.2856936767186716E-49, :coordinates [6 1]} {:distance 2.2010549684107007E-43, :coordinates [2 4]} {:distance 1.3022967824295389E-42, :coordinates [2 2]} {:distance 1.3022967824295389E-42, :coordinates [5 5]} {:distance 4.55898460262208E-41, :coordinates [6 2]} {:distance 4.96473590646251E-31, :coordinates [3 3]}]
)



(defn calculate-min-distance
  [point points]
  (->> (map #(calculate-distance (:coordinates point) (:coordinates %1)) points)
       (apply min)))


;;MAIN


(def potencals (sort-by :distance (getPotentialsVector sourceData)))
(let [firstPotencial (last potencals)]
  (loop [kernels [firstPotencial] elements (drop-last potencals)]
      (let [recalculatedPotencials (sort-by :distance (recalculatePotencials elements (first kernels)))]

       (let [newPotencial (last recalculatedPotencials)]
         (cond
          (> (:distance newPotencial) (* EpsHigh (:distance firstPotencial))) (recur (cons newPotencial kernels) (drop-last recalculatedPotencials))
          (< (:distance newPotencial) (* EpsLow (:distance firstPotencial))) (def result (sort-by :distance kernels))
          (>= (+ (/ (calculate-min-distance newPotencial kernels) Ra) (/ (:distance newPotencial) (:distance firstPotencial))) 1) (recur (cons newPotencial kernels) (drop-last recalculatedPotencials))
          :else (recur kernels (cons (assoc newPotencial :dist 0) (drop-last recalculatedPotencials)))
          )

         )
       )
     )

    (print "Reult:")
    (println result)
  )


