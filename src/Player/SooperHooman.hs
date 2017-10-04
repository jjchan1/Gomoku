module Player.SooperHooman (playerSooperHooman) where

import Types
import Checks

teamMembers :: String 
teamMembers = "Jonathan Chan, Vivian Yung"

playerSooperHooman :: Player 
playerSooperHooman = Player gomokuMinMax "SooperHooman"

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

numInCol :: Move -> Tile -> Board -> Int -> Int
numInCol (row, col) tile board seen
 | (board??(row, col)) == EmptyTile   = seen
 | (board??(row, col)) /= tile        = seen
numInCol (row, col) tile board seen
 | (board??(row+1, col)) == EmptyTile = seen
 | otherwise = numInCol (row+1, col) tile board (seen+1)

numInRow :: Move -> Tile -> Board -> Int -> Int
numInRow (row, col) tile board seen
 | (board??(row, col)) == EmptyTile   = seen
 | (board??(row, col)) /= tile        = seen
numInRow (row, col) tile board seen
 | (board??(row, col+1)) == EmptyTile = seen
 | otherwise = numInRow (row, col+1) tile board (seen+1)

numInDiag :: Move -> Tile -> Board -> Int -> Int
numInDiag (row, col) tile board seen
 | (board??(row, col)) == EmptyTile     = seen
 | (board??(row, col)) /= tile          = seen
numInDiag (row, col) tile board seen
 | (board??(row+1, col+1)) == EmptyTile = seen
 | otherwise = numInDiag (row+1, col+1) tile board (seen+1)

score :: Int -> Int
score x
  | x == 1    = 10^1
  | x == 2    = 10^2
  | x == 3    = 10^4
  | x == 4    = 10^6
  | x == 5    = 10^10
  | otherwise = 0

evaluarTablero :: Tile -> Board -> Int
evaluarTablero tile board = 
  let colScore = map (\row -> sum $ map (\col -> score $ numInCol (row, col) tile board 0) [1..(dimN dim)]) [1..(dimM dim)] in
    let rowScore = map (\row -> sum $ map (\col -> score $ numInRow (row, col) tile board 0) [1..(dimN dim)]) [1..(dimM dim)] in
      let diagScore = map (\row -> sum $ map (\col -> score $ numInDiag (row, col) tile board 0) [1..(dimN dim)]) [1..(dimM dim)] in
        let totalScore = colScore ++ rowScore ++ diagScore in
          sum totalScore


gomokuMinMax :: Tile -> Board -> IO Move
gomokuMinMax tile board
  = return $ snd $ maximum scoredMoves
  where
    scoredMoves = zip scores moves
    scores      = map (evaluarTablero tile . put board tile) moves
    -- scores      = map (evaluateBoardMax tile . put board tile) moves
    -- moves       = validMoves' row col
    moves       = validMoves board

-- evaluateBoardMax' :: Tile -> Board -> Int
-- evaluateBoardMax' tile board =
--   	  case scoreBoard tile board of
--   	  	Just x  -> x
--   	  	Nothing -> let allValidMoves = validMoves' board in 
--   	  	             let newBoards = map (put board (flipTile tile)) allValidMoves in
--   	  				  let allBoardScores = map (evaluateBoardMin (flipTile tile)) newBoards in
--   	  				   minimum allBoardScores

evaluateBoardMax :: Tile -> Board -> Int
evaluateBoardMax tile board =
  	  case scoreBoard tile board of
  	  	Just x  -> x
  	  	Nothing -> let allValidMoves = validMoves board in 
  	  	             let newBoards = map (put board (flipTile tile)) allValidMoves in
  	  				   let allBoardScores = map (evaluateBoardMin (flipTile tile)) newBoards in
  	  				     minimum allBoardScores

evaluateBoardMin :: Tile -> Board -> Int
evaluateBoardMin tile board =
  	  case scoreBoard (flipTile tile) board of
  	  	Just x  -> x
  	  	Nothing -> let allValidMoves = validMoves board in
  	  				 let newBoards = map (put board (flipTile tile)) allValidMoves in
  	  				   let allBoardScores = map (evaluateBoardMax (flipTile tile)) newBoards in
  	  				     maximum allBoardScores 
