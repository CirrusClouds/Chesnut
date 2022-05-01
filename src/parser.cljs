(ns parser)

(def letter-map {\a 0 \b 1 \c 2 \d 3 \e 4 \f 5 \g 6 \h 7})

(def number-map {\1 0 \2 8 \3 16 \4 24 \5 32 \6 40 \7 48 \8 56})

(defn parse-coordinate [coord]
  (let [[col row] (into [] coord)]
    (+ (get letter-map col) (get number-map row))))
