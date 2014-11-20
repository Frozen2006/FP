;; Anything you type in here will be executed
;; immediately with the results shown on the
;; right.
(require '[clojure.string :as str])


(def args *command-line-args*)

;(def args ["C:/butterfly.txt" "Hamming"])
;(def args ["C:/glass.txt" "Hamming"])

(def DistanceName (last args))
(def fileContent (slurp (first args)))

;;CONSTANTS
;(def DistanceName "Hamming")
(def Ra 3)
(def Rb (* 1.5 Ra))
(def EpsHigh 0.5)
(def EpsLow 0.15)


(defn parse-int [s]
   (Integer. (re-find  #"\d+" s )))

(defn parse-float [str]
  (let [n (read-string str)]
       (if (number? n) n nil)))






;;Load file
;(def fileContent (slurp "C:/butterfly.txt"))
(def lines (str/split fileContent #"\r\n"))

(def removieSpace (fn[el](str/replace el " ", "")))
(def getSubvector (fn[el](map parse-float (str/split (removieSpace el) #","))))
(def removieLastEl (fn[el](take (- (count el) 1) el)))


(def sourceData (map removieLastEl (map getSubvector lines)))


;;LOGIC

;;(defn euclidian-distance [c1 c2] (->> (map - c1 c2) (map #(* % %)) (reduce +)))

(defn euclidian-distance [c1 c2] (->> (map - c1 c2) (map #(* % %)) (reduce +) (Math/sqrt)))
(defn hamming-distance [c1 c2] (count (filter true? (map not= c1 c2))))


(def calculate-distance (if (= DistanceName "Hamming") hamming-distance euclidian-distance))

;;(hamming-distance (nth sourceData 0) (nth sourceData 1))

(defn create-point-with-dist  [point dist]  (assoc {:coordinates (into [] point)} :distance dist))


;;GET POTENCIALS
(defn potential
  [distance]
  (Math/exp (- (* (/ 4 (* Ra Ra)) distance))))

(defn revised-potential
  [distance]
  (Math/exp (- (* (/ 4 (* Rb Rb)) distance))))


(defn calc-potencial [item items]
  (reduce + (map #(potential %1) (map #(calculate-distance item %1)  items)))
  )

(defn getPotentialsVector [elements]
  (map #(create-point-with-dist %1 (calc-potencial %1 elements)) elements))





(defn recalcCurrentPotencial [el, kernel] (
                                    assoc el :distance (- (:distance el) (* (:distance kernel) (revised-potential (calculate-distance (:coordinates el) (:coordinates kernel)))))
                                  ))


(defn recalculatePotencials [potencoals kernel]
  (map #(recalcCurrentPotencial %1 kernel) potencoals)
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


;(-main "butterfly.txt" "Hamming")




;;;;;;;;TESTS
;2
(if (=
     (hamming-distance [8 1] [1 3])
     2) (println "Test 1 success!") )

;7.28
(if (=
     (euclidian-distance [8 1] [1 3])
     7.280109889280518) (println "Test 2 success!") )


;1.8222245810143747
(if (=
     (calc-potencial [8 1] [[1 3] [1 5] [8 1]])
     1.8222245810143747) (println "Test 3 success!") )



;1.6599134422277828

(if (=
     (recalcCurrentPotencial {:distance 6.98564016502339, :coordinates [7 6]} {:distance 7.905912556714458, :coordinates [1 3]})
     {:coordinates [7 6] :distance 1.6599134422277828}) (println "Test 4 success!") )


(if (=
     (create-point-with-dist [7 6] 1)
     {:coordinates [7 6] :distance 1}) (println "Test 5 success!") )








