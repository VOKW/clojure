(ns cards.core
  (:require
   [reagent.core :as reagent]
   [re-frame.core :as re-frame]
   [cards.cards-backend :as cards]
   [cards.cards :as c]
   [cards.game :as game]))

(def welcome-message "Memory Game")


(defn game-board []
  [cards/row
   [cards/heading welcome-message]
   [cards/row
    [cards/button "Start game" game/start-game]
    [cards/label "Current player: "
     (c/display-value :current-player)]
    [cards/label "Player 1: "
     (c/display-value [:score 1])]
    [cards/label "Player 2: "
     (c/display-value [:score 2])]]
   [cards/row
    (if (c/display-value :winner)
      (if (= :tie (c/display-value :winner))
        [cards/label "It's a tie!"]
        [cards/label "Player " (c/display-value :winner) " is the winner!!"]))]
   [cards/grid]
   [cards/debug]])

(defn mount-root []
  (re-frame/clear-subscription-cache!)
  (reagent/render [game-board]
                  (.getElementById js/document "app")))

(defn ^:export init []
  (mount-root))
