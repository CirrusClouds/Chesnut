(ns bitboard
  (:require [parser :as parser]))

;; Chess programming generally uses bitboards for performance reasons
;; Chessboards form a monoid
;; History stored via a linked list of chessboards leading up to the present

(def players '(BLACK WHITE))

(def pieces '(WP BP WN BN WB BB WQ BQ WK BK WR BR))

(def modes '(ANALYSIS NORMAL))

(defn empty-chessboard []
  {:player 'WHITE
   :move 1
   :history nil
   :bitboards [{:piece 'WP
                :bitboard 0}
               {:piece 'BP
                :bitboard 0}
               {:piece 'WN
                :bitboard 0}
               {:piece 'BN
                :bitboard 0}
               {:piece 'WB
                :bitboard 0}
               {:piece 'BB
                :bitboard 0}
               {:piece 'WR
                :bitboard 0}
               {:piece 'BR
                :bitboard 0}
               {:piece 'WQ
                :bitboard 0}
               {:piece 'BQ
                :bitboard 0}
               {:piece 'WK
                :bitboard 0}
               {:piece 'BK
                :bitboard 0}
               ]})

;; Check for presence on a number is done with bit-and

(defn square-filled? [i bitboard]
  (not= (js/BigInt 0) (bit-and (js/BigInt (Math/pow 2 i)) (js/BigInt bitboard))))

(defn add-to-bitboard [i bitboard]
  
  (if (square-filled? i bitboard)
    (throw (js/Error. "There's a piece on this square already!"))
    (+ (Math/round (Math/pow 2 i)) bitboard)))


(defn remove-from-bitboard [i bitboard]
  (if (square-filled? i bitboard)
    (- bitboard (Math/round (Math/pow 2 i)))
    (throw (js/Error. "There's no piece on this square!"))))


(defn next-player [player]
  (if (= player 'BLACK)
    'WHITE
    'BLACK))

(defn place-piece-freely [symbol square]
  "Intended for indiscriminate placement whilst updating move + player without checking for correctness. Basically intended to set up positions freely"
  (fn [chessboard]
    (-> chessboard
        (assoc :history chessboard)
        (update :move inc)
        (update :player next-player)
        (update :bitboards (fn [bitboards]
                             (map (fn [bitboard]
                                    (if (= symbol (:piece bitboard))
                                      (update bitboard :bitboard
                                              #(add-to-bitboard (parser/parse-coordinate square) %))
                                      bitboard))
                                  bitboards))))))

(defn move-piece-freely [sq1 sq2]
  "Intended for indiscriminate movements without checking for correctness. Intended for analysis mode"
  (fn [chessboard]
    (-> chessboard
        (assoc :history chessboard)
        (update :move inc)
        (update :player next-player)
        (update :bitboards (fn [bitboards]
                             (map (fn [bitboard]
                                    (if (square-filled? (parser/parse-coordinate sq1) (:bitboard bitboard))
                                      (update bitboard :bitboard
                                              #(add-to-bitboard (parser/parse-coordinate sq2) (remove-from-bitboard (parser/parse-coordinate sq1) %)))
                                      (if (square-filled? (parser/parse-coordinate sq2) (:bitboard bitboard))
                                        (update bitboard :bitboard
                                                #(remove-from-bitboard (parser/parse-coordinate sq2) %))
                                        bitboard)))
                                  bitboards))))))

(defn find-piece-on-square [square]
  "This one's just numbers, not coordinates"
  (fn [chessboard]
    (reduce (fn [acc bitboard]
              (if (square-filled? square (:bitboard bitboard))
                (:piece bitboard)
                acc)) nil (:bitboards chessboard))))


(defn undo []
  (fn [chessboard]
    (:history chessboard)))


(defn starting-board []
  (-> (empty-chessboard)
      ((place-piece-freely 'WN "b1"))
      ((place-piece-freely 'WN "g1"))
      ((place-piece-freely 'BN "b8"))
      ((place-piece-freely 'BN "g8"))
      ((place-piece-freely 'WR "a1"))
      ((place-piece-freely 'WR "h1"))
      ((place-piece-freely 'BR "a8"))
      ((place-piece-freely 'BR "h8"))
      ((place-piece-freely 'WB "c1"))
      ((place-piece-freely 'WB "f1"))
      ((place-piece-freely 'BB "c8"))
      ((place-piece-freely 'BB "f8"))
      ((place-piece-freely 'WK "e1"))
      ((place-piece-freely 'BK "e8"))
      ((place-piece-freely 'WQ "d1"))
      ((place-piece-freely 'BQ "d8"))
      ((place-piece-freely 'WP "a2"))
      ((place-piece-freely 'WP "b2"))
      ((place-piece-freely 'WP "c2"))
      ((place-piece-freely 'WP "d2"))
      ((place-piece-freely 'WP "e2"))
      ((place-piece-freely 'WP "f2"))
      ((place-piece-freely 'WP "g2"))
      ((place-piece-freely 'WP "h2"))
      ((place-piece-freely 'BP "a7"))
      ((place-piece-freely 'BP "b7"))
      ((place-piece-freely 'BP "c7"))
      ((place-piece-freely 'BP "d7"))
      ((place-piece-freely 'BP "e7"))
      ((place-piece-freely 'BP "f7"))
      ((place-piece-freely 'BP "g7"))
      ((place-piece-freely 'BP "h7"))))

(defn print-bitboard [bitboard]
  (map
   (fn [sq]
     (if (square-filled? sq bitboard)
       "o"
       " "))
   (reverse (range 63))))
