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

numInRow :: Move -> Tile -> Board -> Int -> Int
numInRow (row, col) tile board seen
 | (b??(row, col)) == EmptyTile   = seen
 | (b??(row, col)) /= tile        = seen
numInRow (row, col) tile board seen
 | (b??(row, col+1)) == EmptyTile = seen
 | otherwise = numInRow (row, col+1) tile board (seen+1)

numInCol :: Move -> Tile -> Board -> Int -> Int
numInCol (row, col) tile board seen
 | (b??(row, col)) == EmptyTile   = seen
 | (b??(row, col)) /= tile        = seen
numInCol (row, col) tile board seen
 | (b??(row+1, col)) == EmptyTile = seen
 | otherwise = numInCol (row+1, col) tile board (seen+1)

numInDiag :: Move -> Tile -> Board -> Int -> Int
numInDiag (row, col) tile board seen
 | (b??(row, col)) == EmptyTile   = seen
 | (b??(row, col)) /= tile        = seen
numInRow (row, col) tile board seen
 | (b??(row+1, col+1)) == EmptyTile = seen
 | otherwise = numInDiag (row+1, col+1) tile board (seen+1)

scoreBoard :: Tile -> Board -> Int
scoreBoard tile board = sum totalScore
  where rowScore =  map (\row -> sum $ map (\col -> score $ numInRow (row, col) tile board 0) [1..(dimN dim)]) [1..(dimM dim)] in
        colScore =  map (\row -> sum $ map (\col -> score $ numInCol (row, col) tile board 0) [1..(dimN dim)]) [1..(dimM dim)] in
        diagScore =  map (\row -> sum $ map (\col -> score $ numInDiag (row, col) tile board 0) [1..(dimN dim)]) [1..(dimM dim)] in
        totalScore = rowScore ++ colScore ++ diagScore

gomokuMinMax :: Tile -> Board -> IO Move
gomokuMinMax tile board
  = return $ snd $ maximum scoredMoves
  where
    scoredMoves = zip scores moves
    scores      = map (evaluateBoardMax tile . put board tile) moves
    moves       = validMoves' row col
    -- moves       = validMoves board

evaluateBoardMax' :: Tile -> Board -> Int
evaluateBoardMax' tile board =
  	  case scoreBoard tile board of
  	  	Just x  -> x
  	  	Nothing -> let allValidMoves = validMoves' board in 
  	  	             let newBoards = map (put board (flipTile tile)) allValidMoves in
  	  				  let allBoardScores = map (evaluateBoardMin (flipTile tile)) newBoards in
  	  				   minimum allBoardScores

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
