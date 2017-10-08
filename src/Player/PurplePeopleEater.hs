module Player.PurplePeopleEater (playerPurplePeopleEater) where

import Types
import Checks
import Data.List

teamMembers :: String 
teamMembers = "Jonathan Chan, Vivian Yung"

playerPurplePeopleEater :: Player 
playerPurplePeopleEater = Player gomokuMinMax "PurplePeopleEater"

-- to reduce unnecessary search space, only show realistic potential moves
validCols :: Move -> [Move]
validCols (row, col) = [(r, col) | r <- [row-2, row-1, row+1, row+2], r >= 1, r <= 15]

validRows :: Move -> [Move]
validRows (row, col) = [(row, c) | c <- [col-2, col-1, col+1, col+2], c >= 1, c <= 15]

validDiagTLBR :: Move -> [Move]
validDiagTLBR (row, col) = [(r, c) | (r,c) <- [(row-2, col-2), (row-1, col-1), (row+1, col+1), (row+2, col+2)], r >= 1, r <= 15, c >= 1, c <= 15]

validDiagTRBL :: Move -> [Move]
validDiagTRBL (row, col) = [(r, c) | (r,c) <- [(row-2, col+2), (row-1, col+1), (row+1, col-1), (row+2, col-2)], r >= 1, r <= 15, c >= 1, c <= 15]

validMoves' :: Move -> Board -> [Move]
validMoves' (row, col) b = 
  let allPossibleMoves = ((validCols (row, col)) ++ (validRows (row, col)) ++ (validDiagTLBR (row, col)) ++ (validDiagTRBL (row, col))) in
    let allValidMoves = foldl (\a x -> if (b??x) == EmptyTile then a++[x] else a) [] allPossibleMoves in
      allValidMoves

-- find all "tile"-marked positions
-- let potentialMoves = find valid moves surrounding these positions (validMoves')
-- continue with greedy algorithm using potentialMoves
findOldMoves :: Tile -> Board -> [Move]
findOldMoves tile board = [ij | (ij, t) <- board, t == tile]

-- returns all realistic moves
potentialMoves :: Tile -> Board -> [Move]
potentialMoves tile board = 
  let oldMoves = findOldMoves tile board in
    let allValidMoves = (foldl (\acc move -> (validMoves' move board)++acc) [] oldMoves) in
      allValidMoves

-- count number of items in respective search space to use for scoring
-- breaks count on EmptyTile or other tile
numInCol :: Move -> Tile -> Board -> Int -> Int
numInCol (row, col) tile board seen
  | (board??(row, col)) == EmptyTile = seen
  | (board??(row, col)) /= tile      = seen
  | (board??(row, col)) == tile      = numInCol (row+1, col) tile board (seen+1)
  | otherwise                        = seen -- hit edge of board

numInRow :: Move -> Tile -> Board -> Int -> Int
numInRow (row, col) tile board seen
  | (board??(row, col)) == EmptyTile = seen
  | (board??(row, col)) /= tile      = seen
  | (board??(row, col)) == tile      = numInRow (row, col+1) tile board (seen+1)
  | otherwise                        = seen -- hit edge of board

numInDiagTLBR :: Move -> Tile -> Board -> Int -> Int
numInDiagTLBR (row, col) tile board seen
  | (board??(row, col)) == EmptyTile = seen
  | (board??(row, col)) /= tile      = seen
  | (board??(row, col)) == tile      = numInDiagTLBR (row+1, col+1) tile board (seen+1)
  | otherwise                        = seen -- hit edge of board

numInDiagTRBL :: Move -> Tile -> Board -> Int -> Int
numInDiagTRBL (row, col) tile board seen
  | (board??(row, col)) == EmptyTile = seen
  | (board??(row, col)) /= tile      = seen
  | (board??(row, col)) == tile      = numInDiagTRBL (row+1, col-1) tile board (seen+1)
  | otherwise                        = seen -- hit edge of board  

score :: Int -> Int
score x
  | x == 0    = 0
  | x == 1    = 5
  | x == 2    = 20
  | x == 3    = 40
  | x == 4    = 100
  | x == 5    = 9000
  | otherwise = 9001

-- used to generate every row and column permutation
pair_up :: [Int] -> [Int] -> [(Int, Int)]
pair_up xs ys = do
  x <- xs
  y <- ys
  return (x,y)    

-- Given a board with a potential new move placed, calculates the summed (fourple) score of each position
-- returns the maximum summed score  
maxSumFourpleFinder :: Tile -> Board -> Int
maxSumFourpleFinder tile board =
  let positions = pair_up [1..(dimN dim)] [1..(dimM dim)] in
    let fourples = foldl (\scores p -> ((score $ numInCol p tile board 0) +
                        (score $ numInRow p tile board 0) + 
                        (score $ numInDiagTRBL p tile board 0)+
                        (score $ numInDiagTLBR p tile board 0)):scores) [] positions in
      maximum fourples
      
-- reverses the scoredMoves tuple list to allow for move to be used for key search
flipScoredMoves :: [(Int,Move)] -> [(Move,Int)]
flipScoredMoves scoredMoves =
  map (\(score,move) -> (move,score)) scoredMoves

-- find the max (move, score) of every board
findScoredMoves :: Tile -> Board -> [(Move, Int)]
findScoredMoves tile board = scoredMoves 
                               where 
                                 scoredMoves = zip moves scores
                                 scores      = map (maxSumFourpleFinder tile . put board tile) moves
                                 moves       = potentialMoves tile board

-- determine the net gain by taking the score of a potential move and subtracting the previous high score
netGainMoves :: [(Move, Int)] -> Tile -> Board -> [(Move, Int)]
netGainMoves scoredMoves tile board = 
  let currScore = maxSumFourpleFinder tile board in
    let netGains = map (\score -> ((snd score) - currScore, fst score)) scoredMoves in
      flipScoredMoves netGains

-- queries the net score of the other player, using the move as a search key
getOtherNetScore :: Move -> [(Move, Int)] -> Int
getOtherNetScore move otherNetGainMoves = 
  case lookup move otherNetGainMoves of
    Just x -> x
    _ -> 0

-- ?????
watDooooooooooooooo :: [(Move, Int)] -> [(Move, Int)] -> Move
watDooooooooooooooo ownNetGainMoves otherNetGainMoves =
  let ownNetScores = map (\(move,score) -> (score+(getOtherNetScore move otherNetGainMoves),move)) ownNetGainMoves in
    let otherNetScores = map (\(move,score) -> (score+(getOtherNetScore move ownNetGainMoves),move)) otherNetGainMoves in
      let allNetScores = ownNetScores ++ otherNetScores in
        snd $ maximum allNetScores

-- if it is the first move, return the center (8,8)
-- if it is the second move, return the top left diagonal (7,7)
-- otherwise, run a greedy algorithm
-- find the max (score.move) of every board for both players
-- determine the net score of placing a tile in a position by calculating the sum of
-- the net gain for the current player and the net potential loss of the opposing player
-- choose the move that produces the greatest net score
gomokuMinMax :: Tile -> Board -> IO Move
gomokuMinMax tile board 
  | board == emptyBoard                                    = return (8,8)
  | length (validMoves board) == ((dimN dim * dimM dim)-1) = if (board??(8,8)) == EmptyTile then return (8,8)
                                                             else return (7,7)
  | otherwise                                              = return bestMove
                                                               where
                                                                 ownScoredMoves = findScoredMoves tile board
                                                                 otherScoredMoves = findScoredMoves (flipTile tile) board
                                                                 ownNetGains = netGainMoves ownScoredMoves tile board
                                                                 otherNetGains = netGainMoves otherScoredMoves (flipTile tile) board
                                                                 bestMove = watDooooooooooooooo ownNetGains otherNetGains

-- Citations: 

-- Credits go to the paper titled "Go-Moku and Threat-Space Search" by L.V. Allis and M.P.H Huntjens which 
-- helped us determine our overall strategy for creating a Go-Moku player. We learned about the most optimal
-- first and second moves from this paper.

-- Credits also go to Oren Finard (GitHub user Nero144) and Github user guzziksen96. Their implementations of Gomoku 
-- helped us develop a scoring system.  
-- https://raw.githubusercontent.com/Nero144/Gomoku/master/README.txt    
-- https://github.com/guzziksen96/Gomoku/blob/master/Board.hs