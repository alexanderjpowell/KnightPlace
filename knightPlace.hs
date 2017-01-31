-- Alexander Powell
-- Derek O'connell
-- Principles of Programming Languages
-- 04/17/2015

-- knightPlace.hs

------------------------------------------------------------------------

import Data.List


-- Helper functions
replaceAtIndex n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list


-- initialize full board
makeBoard :: [Int] -> [[Int]]
makeBoard lst = (replicate len) [1..len]
	where len = length lst

-- makes a 2-d array of all ones
makeEmpty :: [Int] -> [[Int]]
makeEmpty lst = (replicate len) ((replicate len) 1)
	where len = length lst

lengthOfBoard :: [[Int]] -> Int
lengthOfBoard [] = 0
lengthOfBoard board
	| otherwise = length board

-- the inputted array is the original input (i.e. [4,0,4,4])
-- always enter count of 0 into this function
findQueens :: [Int] -> Int -> [[Int]]
findQueens [] count = []
findQueens (x:xs) count
	| x /= 0 = [[min,count]] ++ findQueens xs (count + 1)
	| otherwise = findQueens xs (count + 1)
	where min = x - 1

-- always enter param n as 0
-- eliminates chess tiles that already have a queen placed on them.  
elimQueens :: [[Int]] -> [[Int]] -> Int -> [[Int]]
elimQueens [] [] 0 = []
elimQueens board queens n
	| n == (length queens) = board
	| otherwise = elimQueens tmp queens (n + 1)
	where tmp = updateMatrix board 0 ((queens!!n!!0), (queens!!n!!1))

-- seemingly changes the 2-d array by actually just returning a new 2-d array
updateMatrix :: [[a]] -> a -> (Int, Int) -> [[a]]
updateMatrix m x (r,c) = 
	take r m ++
	[take c (m !! r) ++ [x] ++ drop (c + 1) (m !! r)] ++
	drop (r + 1) m

-- n should always be 0
elimQMoves_h :: [[Int]] -> Int -> [[Int]]
elimQMoves_h board n
	| otherwise = h
	where [h,v] = [elimHoriz board n, transpose (elimHoriz (transpose board) n)]

-- n should always be 0
elimQMoves_v :: [[Int]] -> Int -> [[Int]]
elimQMoves_v board n
	| otherwise = v
	where [h,v] = [elimHoriz board n, transpose (elimHoriz (transpose board) n)]

combineMat :: [[Int]] -> [Int]
combineMat []   = []
combineMat (x:xs)
	| otherwise = x ++ combineMat xs
	
uncombineMat :: [Int] -> [[Int]]
uncombineMat lst
	| True  = splitEvery factor_size lst
	where factor_size = round (sqrt (fromIntegral (length lst)))

-- h and v are the matrices converted to 1-d arrays using combineMat
multMatrices :: [Int] -> [Int] -> [Int]
multMatrices h v = 
	zipWith (*) h v

elimHoriz :: [[Int]] -> Int -> [[Int]]
elimHoriz board n
	| n == (length board) = board
	| 0 `elem` board!!n = elimHoriz tmp (n + 1)
	| otherwise = elimHoriz board (n + 1)
	where tmp = replaceAtIndex n ((replicate (length board)) 0) board
	
callDiag :: [[Int]] -> [[Int]] -> Int -> [[Int]]
callDiag board queens n
	| n == (length queens) = board
	| otherwise = callDiag (diagDL (diagDR (diagUL (diagUR board (queens!!n!!0) (queens!!n!!1)) (queens!!(n)!!0) (queens!!(n)!!1)) (queens!!(n)!!0) 
								(queens!!(n)!!1)) (queens!!(n)!!0) (queens!!(n)!!1)) queens (n+1)
	
diagUR :: [[Int]] -> Int -> Int -> [[Int]]
diagUR board row col
	| (row == (length board)) || (col == (length board)) = board
	| otherwise = diagUR tmp (row + 1) (col + 1)
	where tmp = updateMatrix board 0 (row, col)
	
diagUL :: [[Int]] -> Int -> Int -> [[Int]]
diagUL board row col
	| (row == (length board)) || (col == -1) = board
	| otherwise = diagUL tmp (row + 1) (col - 1)
	where tmp = updateMatrix board 0 (row, col)
	
diagDR :: [[Int]] -> Int -> Int -> [[Int]]
diagDR board row col
	| (row == -1) || (col == (length board)) = board
	| otherwise = diagDR tmp (row - 1) (col + 1)
	where tmp = updateMatrix board 0 (row, col)
	
diagDL :: [[Int]] -> Int -> Int -> [[Int]]
diagDL board row col
	| (row == -1) || (col == -1) = board
	| otherwise = diagDL tmp (row - 1) (col - 1)
	where tmp = updateMatrix board 0 (row, col)

	
elimKnights :: [[Int]] -> [[Int]] -> Int -> [[Int]]
elimKnights board queens n
	| n == (length queens) = board
	| otherwise = elimKnights (kUL (kLU (kLD (kDL (kDR (kRD (kRU (kUR board (queens!!n!!0) (queens!!n!!1)) (queens!!n!!0) (queens!!n!!1)) (queens!!n!!0) (queens!!n!!1)) (queens!!n!!0) (queens!!n!!1)) (queens!!n!!0) (queens!!n!!1)) (queens!!n!!0) (queens!!n!!1)) (queens!!n!!0) (queens!!n!!1)) (queens!!n!!0) (queens!!n!!1)) queens (n+1)

kUR :: [[Int]] -> Int -> Int -> [[Int]]
kUR board row col
	| (row >= (length board) - 2) || (col >= (length board) - 1) = board
	| otherwise = reverse (updateMatrix (reverse board) 0 ((row + 2), (col + 1)))
	
kRU :: [[Int]] -> Int -> Int -> [[Int]]
kRU board row col
	| (row >= (length board) - 1) || (col >= (length board) - 2) = board
	| otherwise = reverse (updateMatrix (reverse board) 0 ((row + 1), (col + 2)))
	
kRD :: [[Int]] -> Int -> Int -> [[Int]]
kRD board row col
	| (row <= 0) || (col >= (length board) - 2) = board
	| otherwise = reverse (updateMatrix (reverse board) 0 ((row - 1), (col + 2)))
	
kDR :: [[Int]] -> Int -> Int -> [[Int]]
kDR board row col
	| (row <= 1) || (col >= (length board) - 1) = board
	| otherwise = reverse (updateMatrix (reverse board) 0 ((row - 2), (col + 1)))
	
kDL :: [[Int]] -> Int -> Int -> [[Int]]
kDL board row col
	| (row <= 1) || (col <= 0) = board
	| otherwise = reverse (updateMatrix (reverse board) 0 ((row - 2), (col - 1)))

kLD :: [[Int]] -> Int -> Int -> [[Int]]
kLD board row col
	| (row <= 1) || (col <= 0) = board
	| otherwise = reverse (updateMatrix (reverse board) 0 ((row - 1), (col - 2)))

kLU :: [[Int]] -> Int -> Int -> [[Int]]
kLU board row col
	| (row >= (length board) - 1) || (col <= 1) = board
	| otherwise = reverse (updateMatrix (reverse board) 0 ((row + 1), (col - 2)))
	
kUL :: [[Int]] -> Int -> Int -> [[Int]]
kUL board row col
	| (row >= (length board) - 2) || (col <= 0) = board
	| otherwise = reverse (updateMatrix (reverse board) 0 ((row + 2), (col - 1)))



parseCol :: [[Int]] -> [Int] -> Int -> Int -> [Int]
parseCol board outlst r c
	| (r == (length board)) = if (length outlst) == 0
								then [0]
								else outlst
	| (board!!r!!c == 1) = parseCol board (outlst ++ [r+1]) (r+1) c
	| otherwise = parseCol board outlst (r+1) c


-- c is always 0 and final is []
formatBoard :: [[Int]] -> Int -> [[Int]] -> [[Int]]
formatBoard board c final
	| (c == (length board)) = final
	| otherwise = formatBoard board (c + 1) (final ++ [(parseCol board [] 0 c)])

-- Now piece it all together to call one function
--------------------------------------------------------------------

-- function to get through queen positions
queen_pos :: [Int] -> [[Int]]
queen_pos input = reverse (elimQueens (makeEmpty input) (findQueens input 0) 0)

-- function to remover horiz and vert
--input is queen_pos output from prev func
h_and_v :: [[Int]] -> [[Int]]
h_and_v input
	| otherwise = uncombineMat (multMatrices (combineMat h) (combineMat v))
	where [h,v] = [elimQMoves_h input 0, transpose (elimQMoves_h (transpose input) 0)]


allQueens :: [Int] -> [[Int]]
allQueens input = reverse (callDiag (reverse (h_and_v (queen_pos input))) (findQueens input 0) 0)

allKnights :: [Int] -> [[Int]]
allKnights input = elimKnights (reverse (callDiag (reverse (h_and_v (queen_pos input))) (findQueens input 0) 0)) (findQueens input 0) 0

sec2last :: [Int] -> [[Int]]
sec2last input = formatBoard (reverse (allKnights input)) 0 []

knightPlace :: [Int] -> [[Int]]
knightPlace input
	| otherwise = sec2last input





