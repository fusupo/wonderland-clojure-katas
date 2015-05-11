(ns alphabet-cipher.coder
  (:use [clojure.pprint :only (pprint)]))

(def cypher-map (loop [r [[0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25]]]
                  (if (= (count r) (count (first r)))
                    r
                    (recur  (conj r (conj (vec (rest (last r))) (first (last r))))))))

(defn str-to-int [s] (map #(- (- (int %) 32) 65) s))
(defn int-to-str [i] (map #(char (+ 32 (+ % 65))) i))

(defn make-key [kwd msg]
  (map #(nth cypher-map %) (take (count msg) (cycle kwd))))

(defn x-form [mk msg f]
  (clojure.string/join "" (int-to-str (map-indexed f msg))))

(defn base-code [kwd msg f]
  (let [converted-kwd (str-to-int kwd)
        converted-msg (str-to-int msg)]
    (let [mk (make-key converted-kwd converted-msg)]
      (x-form mk converted-msg (partial f mk)))
    ))

(defn encode [keyword message]
  "encodeme"
  (base-code keyword message (fn [mk idx cm] (nth (nth mk idx) cm))))

(defn decode [keyword message]
  "decodeme"
  (base-code keyword message (fn [mk idx cm] (.indexOf (nth mk idx) cm))))




