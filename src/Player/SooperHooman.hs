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

validMoves' :: Board -> Move -> [Move]
validMoves' b (row, col) = 
  let allPossibleMoves = ((validCols (row, col)) ++ (validRows (row, col)) ++ (validDiagTLBR (row, col)) ++ (validDiagTRBL (row, col))) in
   let allValidMoves = foldl (\a x -> if (b??x) == EmptyTile then a++[x] else a) [] allPossibleMoves in
    allValidMoves
 
gomokuMinMax :: Tile -> Board -> IO Move
gomokuMinMax tile board
  = return $ snd $ maximum scoredMoves
  where
    scoredMoves = zip scores moves
    scores      = map (evaluateBoardMax tile . put board tile) moves
    moves       = validMoves' row col

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
