module Players (players) where
import Types
import Player.BestNext (playerBestNext)
import Player.Computer (playerComputer)
import Player.Human (playerHuman)
players :: [(String, Player)]
players = [
  ("BestNext", playerBestNext),("Computer", playerComputer),("Human", playerHuman)
  ]
