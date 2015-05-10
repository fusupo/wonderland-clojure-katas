(ns alphabet-cipher.coder
  (:use [clojure.pprint :only (pprint)]))

(defn encode [keyword message]
  "encodeme"
  (let [cypher_map (loop [r [[0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25]]]
                     (if (= (count r) (count (first r)))
                       r
                       (recur  (conj r (conj (vec (rest (last r))) (first (last r)))))))
        str_to_int (fn [s] (map #(- (- (int %) 32) 65) s))
        int_to_str (fn [i] (map #(char (+ 32 (+ % 65))) i))
        converted_keyword (str_to_int keyword)
        converted_message (str_to_int message)]
    ;; (pprint converted_keyword)
    ;; (pprint converted_message)
    ;; (pprint (take (count converted_message) (cycle (clojure.string/split keyword #""))))
    ;; (pprint (map #(nth cypher_map %) (take (count converted_message) (cycle converted_keyword))))

    (let [mk (map #(nth cypher_map %) (take (count converted_message) (cycle converted_keyword)))]
      ;;(pprint (clojure.string/join "" (int_to_str (map-indexed (fn [idx cm] (nth (nth mk idx) cm)) converted_message))))
      (clojure.string/join "" (int_to_str (map-indexed (fn [idx cm] (nth (nth mk idx) cm)) converted_message)))
      )))

(defn decode [keyword message]
  "decodeme")

;;(= "egsgqwtahuiljgs" (encode "scones" "meetmebythetree"))
;; (= ""meetmebythetree" (decode "scones" "egsgqwtahuiljgs")










