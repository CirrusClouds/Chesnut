
(ns game
  (:require [utils :include-macros true :refer [slurp]]
            [cljs.reader :refer [read-string]]
            [reagent.core :as r]
            ["ink" :refer [render Text Box Newline useInput useApp]]
            [bitboard :as bb]
            [parser :as cc]))


(def opening-data (read-string (slurp "resources/openings.edn")))

(def opening-path (r/atom []))

(def board (r/atom (-> (bb/starting-board)
                       (assoc :move 1))))

(def sym-string-map-basic {'WN "N"
                            'BN "N"
                            'WB "B"
                            'BB "B"
                            'WK "K"
                            'BK "K"
                            'WQ "Q"
                            'BQ "Q"
                            'WR "R"
                            'BR "R"})

(def sym-string-map {nil [:> Text "   "]
                     'WN [:> Text {:bold true } " N "]
                     'BN [:> Text {:bold true :color "grey"} " N "]
                     'WB [:> Text {:bold true} " B "]
                     'BB [:> Text {:bold true :color "grey"} " B "]
                     'WR [:> Text {:bold true} " R "]
                     'BR [:> Text {:bold true :color "grey"} " R "]
                     'WK [:> Text {:bold true} " K "]
                     'BK [:> Text {:bold true :color "grey"} " K "]
                     'WP [:> Text {:bold true} " P "]
                     'BP [:> Text {:bold true :color "grey"} " P "]
                     'WQ [:> Text {:bold true} " Q "]
                     'BQ [:> Text {:bold true :color "grey"} " Q "]
                     })

(defn render-options []
  (let [options-map
        (-> (get-in opening-data @opening-path)
            (dissoc :move)
            (dissoc :content)
            (dissoc :piece))]
    [:> Box {:flex-direction "column"}
     [:> Text "Moves with analysis"]
     (map (fn [option]
            (let [sym-for-option ((bb/find-piece-on-square (cc/parse-coordinate (first (:move (nth option 1))))) @board)]
              [:> Box
               [:> Text (first option) ":  "]
               (if (and
                    (not= 'BP sym-for-option)
                    (not= 'WP sym-for-option))
                 [:> Text (get sym-string-map-basic sym-for-option)
                  (nth (:move (nth option 1)) 1)]
                 [:> Text (nth (:move (nth option 1)) 1)])])) options-map)])) 

(defn render-chessboard []
  [:> Box {:flex-direction "column" :margin-left 1}
   [:> Box {:flex-direction "column" :border-style "single" :width 51}
    (map (fn [row]
           [:> Box {:padding-top 0} 
            (map (fn [col]
                   (let [color (if (odd? (+ row col))
                                 "white"
                                 "grey")]
                     [:> Box {:margin-left 1 :border-style "bold" :border-color color}
                      (get sym-string-map ((bb/find-piece-on-square (+ (* 8 row) col))
                                           @board))]))
                 (range 8))])
         (reverse (range 8)))]
   [:> Newline]
   [:> Box {:flex-direction "column" :width 64}
    (if (:content (get-in opening-data @opening-path))
      (map-indexed (fn [i paragraph]
                     (if (= i 0)
                       [:> Text {:bold true} paragraph]
                       [:> Text paragraph]))
                   (:content (get-in opening-data @opening-path)))
      [:> Text "No analysis found, please go back"])
    [:> Newline]
    (render-options)]])


(defn render-analysis []
  (let [exit (useApp)]
    (useInput (fn [input key]
                (cond
                  (.-escape key) ((.-exit exit))
                  (= input "1") (do
                                  (swap! opening-path #(conj % "1"))
                                  (swap! board (bb/move-piece-freely
                                                (nth (:move (get-in opening-data @opening-path)) 0)
                                                (nth (:move (get-in opening-data @opening-path)) 1))))
                  (= input "2") (do
                                  (swap! opening-path #(conj % "2"))
                                  (swap! board (bb/move-piece-freely
                                                (nth (:move (get-in opening-data @opening-path)) 0)
                                                (nth (:move (get-in opening-data @opening-path)) 1))))
                  (= input "3") (do
                                  (swap! opening-path #(conj % "3"))
                                  (swap! board (bb/move-piece-freely
                                                (nth (:move (get-in opening-data @opening-path)) 0)
                                                (nth (:move (get-in opening-data @opening-path)) 1))))
                  (= input "4") (do
                                  (swap! opening-path #(conj % "4"))
                                  (swap! board (bb/move-piece-freely
                                                (nth (:move (get-in opening-data @opening-path)) 0)
                                                (nth (:move (get-in opening-data @opening-path)) 1))))
                  (= input "5") (do
                                  (swap! opening-path #(conj % "5"))
                                  (swap! board (bb/move-piece-freely
                                                (nth (:move (get-in opening-data @opening-path)) 0)
                                                (nth (:move (get-in opening-data @opening-path)) 1))))
                  (= input "6") (do
                                  (swap! opening-path #(conj % "6"))
                                  (swap! board (bb/move-piece-freely
                                                (nth (:move (get-in opening-data @opening-path)) 0)
                                                (nth (:move (get-in opening-data @opening-path)) 1))))
                  (= input "7") (do
                                  (swap! opening-path #(conj % "7"))
                                  (swap! board (bb/move-piece-freely
                                                (nth (:move (get-in opening-data @opening-path)) 0)
                                                (nth (:move (get-in opening-data @opening-path)) 1))))

                  (= input "u") (do
                                  (when (not= 1 (:move @board))
                                    (swap! board (bb/undo)))
                                  (swap! opening-path #(vec (drop-last %)))))))
    
    [:f> render-chessboard]
    ))


(defn main [& opts]
  (if (= (first opts) "help")
    (do
      (prn "Useage")
      (prn "Press the numbers to cycle through analysis")
      (prn "Press u to undo and go back to a previous analysis state")
      (prn "Press esc to quit")
      (prn "Openings and their analyses are stored in openings.edn"))
    (render (r/as-element [:f> render-analysis]))))
