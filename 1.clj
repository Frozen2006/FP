;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.
(require '[clojure.string :as str])


;;CONSTANTS
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
;;(for [d lines] (getSubvector d ))

(def sourceData (map removieLastEl (map getSubvector lines)))


;;LOGIC

(defn euclidian-distance [c1 c2] (->> (map - c1 c2) (map #(* % %)) (reduce +)))
(defn hamming-distance [c1 c2] (count (filter true? (map not= c1 c2))))

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
  (potential (reduce + (map #(hamming-distance item %1) items)))
  )


(defn getPotentialsVector [elements]
  (map #(create-point-with-dist %1 (calc-potencial %1 elements)) elements))





(defn recalcCurrentPotencial [el, kernel] (
                                    assoc el :distance (- (:distance el) (* (:distance kernel) (revised-potential (hamming-distance el kernel))))
                                  ))


(defn recalculatePotencials [potencoals kernel]
  (map #(recalcCurrentPotencial %1 kernel) potencoals)
  )



(defn calculate-min-distance
  [point points]
  (->> (map #(hamming-distance (:coordinates point) (:coordinates %1)) points)
       (apply min)))


;;MAIN


(def potencals (sort-by :distance (getPotentialsVector sourceData)))
(let [firstPotencial (last potencals)]
  (loop [kernels [firstPotencial] elements (drop-last potencals)]
     (let [recalculatedPotencials (sort-by :distance (recalculatePotencials elements (first kernels)))]

       (let [newPotencial (last recalculatedPotencials)]
         (cond
          (> (:distance newPotencial) (* EpsHigh (:distance firstPotencial))) (recur (cons newPotencial kernels) (drop-last recalculatedPotencials))
          (< (:distance newPotencial) (* EpsLow (:distance firstPotencial))) (sort-by :distance kernels)
          (>= (+ (/ (calculate-min-distance newPotencial kernels) Ra) (/ (:distance newPotencial) (:distance firstPotencial))) 1) (recur (cons newPotencial kernels) (drop-last recalculatedPotencials))
          :else (recur kernels (cons (assoc newPotencial :dist 0) (drop-last recalculatedPotencials)))
          )

         )
       )
     )


  )


