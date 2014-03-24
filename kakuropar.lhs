>import System.Environment
>import System.IO
>import System.IO.Error
>import Data.List
>import Data.Maybe
>import System.Exit
>import Control.Parallel.Strategies

------------------------------------------------------
Kakuro Solver : concurrent version
by Caroline Van den Hauwe
------------------------------------------------------
The algorithm and explanation start at line 96, titled 'ALGORITHM: RECURSIVE BACKTRACKING'.

HOW TO RUN

./kakuro <filename> <width>

with <filename> the path to a text file containing a kakuro puzzle, 
structured as explained in "DATA TYPES" 
and <width> optional, the width in case given puzzle is not square

DATA TYPES

Every cell in a puzzle is represented as a Tile, 
which is a tuple of three Int's: (value,horizontal,vertical).
value is -2 if a cell is not part of the puzzle,
		 -1 if a cell is part of the puzzles problem:
			then horizontal contains the expected sum for the following row
			and/or vertical contains the expected sum for the column below
		 0 if is yet to be filled in
		 1 to 9 if it has been filled in
		
>type Tile = (Int,Int,Int)

A (part of a) kakuro puzzle is thus an array of Tiles: [Tile]

>type KakuroPart = [Tile]
>type Kakuro = [Tile]

For example, the simple kakuro
		7\ 6\
	4\4 __	__
\7	__	__	__
\6	__	__	__

is internally represented as [(-2,0,0),(-2,0,0),(-1,0,7),(-1,0,6)(-2,0,0),(-1,4,4),(0,0,0),(0,0,0),(-1,7,0),(0,0,0),(0,0,0),(0,0,0),(-1,6,0),(0,0,0),(0,0,0),(0,0,0)]

>posVal = [1..9]

FILE HANDLING

Following methods handle processing arguments and reading a puzzle file,
before launching solve which starts the actual solving.

>main = toTry `catch` handler  
             
>toTry :: IO ()
>toTry = do
>	progName <- getProgName
>	args <- getArgs
>	case args of
>		(fileName : w : _) 	-> do
>								contents <- readFile fileName
>								let readwidth = read w
>					 			let readcontents = read contents :: Kakuro
>								let lengthRC = length readcontents
>								case solve [] readcontents lengthRC readwidth of
>									Nothing -> putStrLn $ "No solution was found. Perhaps " ++
>										fileName ++ " is not square and its width was not provided: \nUsage: " ++
>										progName ++ " <kakuro file> <kakuro width in case it is not square>"
>									Just a 	-> print a
>		(fileName : _) 		-> do
>								contents <- readFile fileName
>								let readcontents = read contents :: Kakuro
>								let lengthRC = length readcontents
>								case solve [] readcontents lengthRC (isqrt lengthRC) of
>									Nothing -> putStrLn $ "No solution was found. Perhaps " ++
>										fileName ++ " is not square and its width was not provided: \nUsage: " ++
>										progName ++ " <kakuro file> <kakuro width in case it is not square>"
>									Just a 	-> print a		
>		_ 					-> do
>								putStrLn $ "\nUsage: " ++ progName ++ " <kakuro file> <kakuro width in case it is not square>\n"
>								putStrLn inputhelp
>								exitFailure
 
>handler :: IOError -> IO ()  
>handler e  
>    | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
>    | otherwise = ioError e


ALGORITHM: RECURSIVE BACKTRACKING - branching

solve takes a KakuroPart 'puzzleDone' with all processed Tiles, 
a KakuroPart 'puzzleRest' with all Tiles yet to be done,
the puzzle length and the puzzle width.

>solve :: KakuroPart -> KakuroPart -> Int -> Int -> Maybe Kakuro
>solve puzzleDone [] _ _ = Just $ reverse puzzleDone

solve inspects the value Int (cfr. DATA TYPES) of the first Tile in puzzleRest.

>solve puzzleDone ((a,b,c):xs) puzzleLength puzzleSide = 
>	case a of

If this value is -2 or -1, the current cell can be ignored. It is placed in front of puzzleDone
and removed from puzzleRest in the next recursive call of solve.

>		-2	-> solve ((a,b,c):puzzleDone) xs puzzleLength puzzleSide
>		-1	-> solve ((a,b,c):puzzleDone) xs puzzleLength puzzleSide

If however, this value is 0,

>		0	-> do

a list of possible values for this cell is calculated using okValues

>				let okVals = okValues puzzleDone ((a,b,c):xs) puzzleSide

and for every possible value a new branch in the recursion tree of solve is started. 

>				case catMaybes $ parMap rdeepseq (\val -> solve ((val,0,0):puzzleDone) xs puzzleLength puzzleSide) okVals of

If our solve has not led to a solution, Nothing is returned and this branch of the execution ends.

>					[]		-> Nothing

If solve does lead to a solution, it is returned.

>					(s:ss)	-> Just s


CALCULATING POSSIBLE VALUES - bounding

The possible values for a cell are calculated by calculating the possible values according to the row
and intersecting this list with the possible values according to the column.

>okValues :: KakuroPart -> KakuroPart -> Int -> [Int]
>okValues puzzleDone puzzleRest puzzleSide = rowv `intersect` columnv
>	where
>		rowv = rowValues puzzleDone puzzleRest
>		columnv = columnValues puzzleDone puzzleRest puzzleSide

- ROW

To calculate the possible values according to the row, we first look at the processed part of our puzzle.
As this part of our puzzle is in reverse order, our last reviewed cell is in front.

>rowValuesDone :: KakuroPart -> Int -> Int -> [Int] -> (Int,Int,[Int])
>rowValuesDone [] sumv zeros vals = (sumv,zeros,vals)
>rowValuesDone _ sumv zeros [] = (sumv,zeros,[])

Therefore, we simply review the value of the first Tile.

>rowValuesDone ((a,b,c):xs) sumv zeros vals =  case a of

If it is -2, we stop. If it is -1 we add its value to our sumv and stop.
If it is 0, we call this function recursively and remember we encountered an empty cell.

>										-2 	-> (sumv,zeros,vals)
>										-1 	-> (sumv+b,zeros,vals)
>										0	-> rowValuesDone xs sumv (zeros+1) vals

If the cell already contains a value, this value is substracted from our sum
and removed from our list of possibly values, thereby avoiding duplicate values.

>										_	-> rowValuesDone xs (sumv-a) zeros $ filter (/=a) vals

Our function will return the remaining sum of the row, the amount of empty cells and the possible values.

We repeat the same process at the other remaining part of our puzzle.

>rowValuesLeft :: KakuroPart -> Int -> Int -> [Int] -> (Int,Int,[Int])
>rowValuesLeft [] sumv zeros vals = (sumv,zeros,vals)
>rowValuesLeft _ sumv zeros [] = (sumv,zeros,[])
>rowValuesLeft ((a,b,c):xs) sumv zeros vals = case a of
>										-2	-> (sumv,zeros,vals)
>										-1	-> (sumv,zeros,vals)
>										0	-> rowValuesLeft xs sumv (zeros+1) vals
>										_	-> rowValuesLeft xs (sumv-a) zeros $ filter (/=a) vals

These two results are brought together in rowValues. The remaining sums are added,
the amounts of empty cells are added. The possible values are intersected.

>rowValues :: KakuroPart -> KakuroPart -> [Int]
>rowValues puzzleDone puzzleRest = case zerosL+zerosR of

If there is only one empty cell left, our current cell,
the biggest possible value is returned as only possible value.

>									1	-> if null filtered 
>											then [] 
>											else filter (\l -> l == (sumL+sumR)) [last filtered]

If there are several empty cells, the possible values are reduced once more
taking into account possible sets of values that can correctly fill in the remainder of the row.

>									_	-> okPermute filtered (zerosL+zerosR) (sumL+sumR)
>	where
>		filtered = filter (<=sumL+sumR) $ intersect valsL valsR
>		(sumL,zerosL,valsL) = rowValuesDone puzzleDone 0 0 posVal
>		(sumR,zerosR,valsR) = rowValuesLeft puzzleRest 0 0 posVal

To do so, permute takes a length and a list and returns permutations of this list of given length.

>permute :: (Num a) => a -> [b] -> [[b]]
>permute 0 _ = [[]]
>permute length list = [x:xs | x:xs' <- tails list, xs <- permute (length-1) xs']

okPermute removes from these permutations those that do not achieve the sum,
puts all their values in one list and removes duplicates.

>okPermute :: [Int] -> Int -> Int -> [Int]
>okPermute okVal zeros sumv = nub $ concat $ filter (\l -> sum l == sumv) $ permute zeros okVal

- COLUMN

Calculation of possible values according to the column uses functions highly similar to those for the row.
However, because cells are now above (instead of next to) each other,
we only have to take cells into account that are exactly puzzleWidth-1 cells apart.
Therefore, drop (puzzleSide-1) ignores unnecessary cells at each recursion.

>columnValuesDone :: KakuroPart -> Int -> Int -> Int -> [Int] -> (Int,Int,[Int])
>columnValuesDone puzzleDone puzzleSide sumv zeros vals = columnValuesDone' (drop (puzzleSide-1) puzzleDone) puzzleSide sumv zeros posVal

>columnValuesDone' :: KakuroPart -> Int -> Int -> Int -> [Int] -> (Int,Int,[Int])
>columnValuesDone' [] _ sumv zeros vals = (sumv,zeros,vals)
>columnValuesDone' _ _ sumv zeros [] = (sumv,zeros,[])
>columnValuesDone' ((a,_,c):xs) puzzleSide sumv zeros vals = 
>	case a of
>			-2	-> (sumv,zeros,vals)
>			-1	-> (sumv+c,zeros,vals)
>			0	-> columnValuesDone' (drop (puzzleSide-1) xs) puzzleSide sumv (zeros+1) vals
>			_	-> columnValuesDone' (drop (puzzleSide-1) xs) puzzleSide (sumv-a) zeros $ filter (/=a) vals
							
>columnValuesLeft :: KakuroPart -> Int -> Int -> Int -> [Int] -> (Int,Int,[Int])
>columnValuesLeft [] _ sumv zeros vals = (sumv,zeros,vals)
>columnValuesLeft _ _ sumv zeros [] = (sumv,zeros,[])
>columnValuesLeft ((a,b,c):xs) puzzleSide sumv zeros vals = 
>	case a of
>			-2	-> (sumv,zeros,vals)
>			-1	-> (sumv,zeros,vals)
>			0	-> columnValuesLeft (drop (puzzleSide-1) xs) puzzleSide sumv (zeros+1) vals
>			_	-> columnValuesLeft (drop (puzzleSide-1) xs) puzzleSide (sumv-a) zeros $ filter (/=a) vals	

>columnValues :: KakuroPart -> KakuroPart -> Int -> [Int]
>columnValues puzzleDone puzzleRest puzzleSide = case zerosL+zerosR of
>										1	-> if null filtered 
>												then [] 
>												else filter (\l -> l == (sumL+sumR))  [last filtered]
>										_	-> okPermute filtered (zerosL+zerosR) (sumL+sumR)
>		where
>			filtered = filter (<=sumL+sumR) $ intersect valsL valsR
>			(sumL,zerosL,valsL) = columnValuesDone puzzleDone puzzleSide 0 0 posVal
>			(sumR,zerosR,valsR) = columnValuesLeft puzzleRest puzzleSide 0 0 posVal
								

MISCELLANIOUS HELP

When a width was not given, the puzzle is assumed to be square and its width is calculated as follows:

>isqrt :: (Integral a) => a -> a
>isqrt = floor . sqrt . fromIntegral

If the solver fails to read the input, a help text is displayed.

>inputhelp = "A correct <kakuro file> is structured as follows:\t[(-2,0,0),(-1,6,0), ... ,(9,0,0)]\nIn which every (value,horizontal,vertical) represents a tile/cell of the puzzle. \n <value> can be\n\t-2\t-> the tile is not part of the puzzle\n\t-1\t-> the tile is part of the puzzle problem and contains one or more sums (see horizontal and vertical)\n\t0\t-> the tile still has to be filled in\n\t[1..9]\t-> the tile has received a value\n <horizontal> holds the sum the following tiles/cells in the row need to achieve\n <vertical> holds the sum the following tiles/cells in the column need to achieve"