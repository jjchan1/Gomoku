module Player.SooperHooman (playerSooperHooman) where

import Types
import Checks

teamMembers :: String 
teamMembers = "Jonathan Chan, Vivian Yung"

playerSooperHooman :: Player 
playerSooperHooman = Player gomokuMinMax'' "SooperHooman"

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
  | x == 1    = 5
  | x == 2    = 20
  | x == 3    = 40
  | x == 4    = 100
  | x == 5    = 1000
  | otherwise = 0

pair_up :: [Int] -> [Int] -> [(Int, Int)]
pair_up x y = 
  let pairs = map (\p -> map (\q -> (p,q)) y) x in
    concat pairs

pair_up' :: [Int] -> [Int] -> [(Int, Int)]
pair_up' xs ys = do
  x <- xs
  y <- ys
  return (x,y)    

-- evaluarTablero :: Tile -> Board -> Int
-- evaluarTablero tile board = 
--   let colScore = map (\row -> map (\col -> score $ numInCol (row, col) tile board 0) [1..(dimN dim)]) [1..(dimM dim)] in
--     let rowScore = map (\row -> map (\col -> score $ numInRow (row, col) tile board 0) [1..(dimN dim)]) [1..(dimM dim)] in
--       let diagScoreTLBR = map (\row -> map (\col -> score $ numInDiagTLBR (row, col) tile board 0) [1..(dimN dim)]) [1..(dimM dim)] in
--         let diagScoreTRBL = map (\row -> map (\col -> score $ numInDiagTRBL (row, col) tile board 0) [1..(dimN dim)]) [1..(dimM dim)] in
--           let totalScore = colScore ++ rowScore ++ diagScoreTLBR ++ diagScoreTRBL in
--             sum totalScore

evaluarTablero' :: Tile -> Board -> Int
evaluarTablero' tile board =
  let positions = pair_up' [1..(dimN dim)] [1..(dimM dim)] in
    let fourples = foldl (\scores p -> ((score $ numInCol p tile board 0) +
                        (score $ numInRow p tile board 0) + 
    	                  (score $ numInDiagTRBL p tile board 0)+
    	                  (score $ numInDiagTLBR p tile board 0)):scores) [] positions in
      maximum fourples

-- evaluarTablero :: Tile -> Board -> Int
-- evaluarTablero tile board = 
--   let colScore = map (\row -> sum $ map (\col -> score $ numInCol (row, col) tile board 0) [1..(dimN dim)]) [1..(dimM dim)] in
--     let rowScore = map (\row -> sum $ map (\col -> score $ numInRow (row, col) tile board 0) [1..(dimN dim)]) [1..(dimM dim)] in
--       let diagScoreTLBR = map (\row -> sum $ map (\col -> score $ numInDiagTLBR (row, col) tile board 0) [1..(dimN dim)]) [1..(dimM dim)] in
--         let diagScoreTRBL = map (\row -> sum $ map (\col -> score $ numInDiagTRBL (row, col) tile board 0) [1..(dimN dim)]) [1..(dimM dim)] in
--           let totalScore = colScore ++ rowScore ++ diagScoreTLBR ++ diagScoreTRBL in
--             sum totalScore


-- gomokuMinMax :: Tile -> Board -> IO Move
-- gomokuMinMax tile board
--   = return $ snd $ maximum scoredMoves
--   where
--     scoredMoves = zip scores moves
--     scores      = map (evaluarTablero tile . put board tile) moves
--     -- scores      = map (evaluateBoardMax tile . put board tile) moves
--     -- moves       = validMoves' row col
--     moves       = validMoves board

-- gomokuMinMax' :: Tile -> Board -> IO Move
-- gomokuMinMax' tile board =
--   let moves = validMoves board in
--     let scores = map (evaluarTablero tile . put board tile) moves in
--       let scoredMoves = zip scores moves in
-- 	    if mod (length (validMoves board)) 2 == 0 then return $ snd $ maximum scoredMoves
-- 	    else return $ snd $ minimum scoredMoves

-- gomokuMinMax'' :: Tile -> Board -> IO Move
-- gomokuMinMax'' tile board =
-- 	if board == emptyBoard then return (8,8)
-- 	else if length (validMoves board) == ((dimN * dimM)-1) then
-- 	  if (board??(8,8)) == EmptyTile then return (8,8)
-- 	  else return (7,7)
-- 	else return $ snd $ maximum scoredMoves
-- 	  where
-- 	    scoredMoves = zip scores moves
-- 	    scores      = map (evaluarTablero tile . put board tile) moves
-- 	    -- scores      = map (evaluateBoardMax tile . put board tile) moves
-- 	    -- moves       = validMoves' row col
-- 	    moves       = validMoves board

gomokuMinMax'' :: Tile -> Board -> IO Move
gomokuMinMax'' tile board 
  | board == emptyBoard                            = return (8,8)
  | length (validMoves board) == ((dimN dim * dimM dim)-1) = if (board??(8,8)) == EmptyTile then return (8,8)
  	                                                 else return (7,7)
  | otherwise            	                         = return $ snd $ maximum scoredMoves
													   where
													     scoredMoves = zip scores moves
													     scores      = map (evaluarTablero' tile . put board tile) moves
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
