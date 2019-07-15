(ns cards.game
  (:require-macros [cards.cards :as cards])
  (:require [cards.cards :as cards]))

(def cards["A" "B" "C" "D"])
(def cards2 (concat cards cards cards))

(defn up? [card]
  (= :front (get card :side)))
  
(defn up-cards [grid]
  (filter (fn [pos-card]
            (let [pos (key pos-card)
                  card(val pos-card)]
             (up? card)))
    grid))
     


(defn start-game []
  (cards/remove-all-cards)
  (cards/set-value :current-player 1)
  (cards/set-value [:score 1] 0)
  (cards/set-value [:score 2] 0)
  (cards/delete-value :winner)
  (let [scards (shuffle cards2)]
   (dotimes [i (count scards)]
     (let [x (mod i 4)
           y (quot i 4)
           picture (nth scards i)]
       (cards/deal x y picture)))))
          
  

(defn on-click
  "This function will be called when a grid item is clicked."
  [x y card grid values]
  (if (not (up? card))
    (if (< (count(up-cards grid)) 3)
      (do
        (cards/flip x y)
        (if ( = 2 (count (up-cards grid)))
          (cards/end-turn)))))
  (prn grid))

  



(defn on-turn-end
  "This function will be called when the turn changes."
  [grid values]
  (prn :turn)
  (let [pos-cards (up-cards grid)
        
        pos-card1 (first pos-cards)
        pos1 (key pos-card1)
        x1 (first pos1)
        y1 (second pos1)
        card1 (val pos-card1)
        picture1 (get card1 :picture)
        
        pos-card2 (second pos-cards)
        pos2 (key pos-card2)
        x2 (first pos2)
        y2 (second pos2)
        card2 (val pos-card2)
        picture2 (get card2 :picture)
        
        pos-card3 (nth pos-cards 2)
        pos3 (key pos-card3)
        x3 (first pos3)
        y3 (second pos3)
        card3 (val pos-card3)
        picture3 (get card3 :picture)]
    
    (if (= picture1 picture2 picture3)
      (do
        (cards/wait 2
         (cards/remove-card x1 y1)
         (cards/remove-card x2 y2)
         (cards/remove-card x3 y3)          
         (let [current-player (get values :current-player)
               score (get values [:score current-player])]
           (cards/set-value [:score current-player] (+ 1 score))
           (if (= 3 (count grid))
             (cards/end-game)))))
   
      (do
        (cards/wait 2
          (cards/flip x1 y1)
          (cards/flip x2 y2)
          (cards/flip x3 y3)
          (if (= 1 (get values :current-player))
            (cards/set-value :current-player 2)
            (cards/set-value :current-player 1)))))))
    
  
       
  
  
  

(defn on-game-end
  "This function will be called when the game ends."
  [grid values]
  (let [p1-score (get values [:score 1])
        p2-score (get values [:score 2])]
    (if (> p1-score p2-score)
      (cards/set-value :winner 1)
      (if (< p1-score p2-score)
        (cards/set-value :winner 2)
        (cards/set-value :winner :tie)))))
  
