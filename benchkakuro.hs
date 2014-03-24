import System.Environment
import System.IO
import System.IO.Error
import Data.List
import Data.Maybe
import System.Exit
import Debug.Trace
import Criterion.Main (defaultMain)
import Criterion (bench, nf)
import Control.Parallel.Strategies

---------------------------------------------------------------------------------------------------------------
--Kakuro Solver : Criterion benchmark version (please refer to kakuro.lhs and kakuropar.hls for commented code)
--by Caroline Van den Hauwe
---------------------------------------------------------------------------------------------------------------


type Tile = (Int,Int,Int)
type KakuroHalf = [Tile]
type Kakuro = [Tile]

posVal = [1..9]

main :: IO ()
main = do
    kakuro <- readKakuro "25x35.txt"
    defaultMain
        [ bench "solve"    $ nf solve'    kakuro
        , bench "solvePar" $ nf solvePar' kakuro
        ]
  where
    solve' (x, y, z, w) = solve x y z w
    solvePar' (x, y, z, w) = solvePar x y z w

readKakuro :: FilePath -> IO ([Tile], [Tile], Int, Int)
readKakuro filePath = 
	do 
		rf <- readFile filePath
		let readPuzzle = read rf :: Kakuro
		let puzzleLen = length readPuzzle
		let puzzleWidth = 25
		return ([], readPuzzle, puzzleLen, puzzleWidth)
		
				

-- PROGRAM INITIALISATION, FILE HANDLING
oldMain = toTry `catch` handler  
              
toTry :: IO ()
toTry = do
	progName <- getProgName
	args <- getArgs
	case args of
		(fileName : w : _) 	-> do
								contents <- readFile fileName
								let readwidth = read w
					 			let readcontents = read contents :: Kakuro
								let lengthRC = length readcontents
								case solve [] readcontents lengthRC readwidth of
									Nothing -> putStrLn $ "No solution was found. Perhaps " ++ fileName ++ " is not square and its width was not provided: \nUsage: " ++ progName ++ " <kakuro file> <kakuro width in case it is not square>"
									Just a 	-> print a
		(fileName : _) 		-> do
								contents <- readFile fileName
								let readcontents = read contents :: Kakuro
								let lengthRC = length readcontents
								case solve [] readcontents lengthRC (isqrt lengthRC) of
									Nothing -> putStrLn $ "No solution was found. Perhaps " ++ fileName ++ " is not square and its width was not provided: \nUsage: " ++ progName ++ " <kakuro file> <kakuro width in case it is not square>"
									Just a 	-> print a		
		_ 					-> do
								putStrLn $ "\nUsage: " ++ progName ++ " <kakuro file> <kakuro width in case it is not square>\n"
								putStrLn inputhelp
								exitFailure
  
handler :: IOError -> IO ()  
handler e  
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"  
    | otherwise = ioError e

solve :: KakuroHalf -> KakuroHalf -> Int -> Int -> Maybe Kakuro
solve puzzleDone [] _ _ = Just $ reverse puzzleDone
solve puzzleDone ((a,b,c):xs) puzzleLength puzzleSide = 
	case a of
		-2	-> solve ((a,b,c):puzzleDone) xs puzzleLength puzzleSide
		-1	-> solve ((a,b,c):puzzleDone) xs puzzleLength puzzleSide
		0	-> do
				let okVals = okValues puzzleDone ((a,b,c):xs) puzzleSide
				case catMaybes $ map (\val -> solve ((val,0,0):puzzleDone) xs puzzleLength puzzleSide) okVals of
					[]		-> Nothing
					(s:ss)	-> Just s
					
solvePar :: KakuroHalf -> KakuroHalf -> Int -> Int -> Maybe Kakuro
solvePar puzzleDone [] _ _ = Just $ reverse puzzleDone
solvePar puzzleDone ((a,b,c):xs) puzzleLength puzzleSide = 
	case a of
		-2	-> solvePar ((a,b,c):puzzleDone) xs puzzleLength puzzleSide
		-1	-> solvePar ((a,b,c):puzzleDone) xs puzzleLength puzzleSide
		0	-> do
				let okVals = okValues puzzleDone ((a,b,c):xs) puzzleSide
				case catMaybes $ parMap rdeepseq (\val -> solvePar ((val,0,0):puzzleDone) xs puzzleLength puzzleSide) okVals of
					[]		-> Nothing
					(s:ss)	-> Just s
										
okPermute :: [Int] -> Int -> Int -> [Int]
okPermute okVal zeros sumv = nub $ concat $ filter (\l -> sum l == sumv) $ permute zeros okVal

permute :: (Num a) => a -> [b] -> [[b]]
permute 0 _ = [[]]
permute length list = [x:xs | x:xs' <- tails list, xs <- permute (length-1) xs']

-- CALCULATING POSSIBLE VALUES (incl. bounding)
okValues :: KakuroHalf -> KakuroHalf -> Int -> [Int]
okValues puzzleDone puzzleRest puzzleSide = rowv `intersect` columnv
	where
		rowv = rowValues puzzleDone puzzleRest
		columnv = columnValues puzzleDone puzzleRest puzzleSide

-- FOR ROW
rowValues :: KakuroHalf -> KakuroHalf -> [Int]
rowValues puzzleDone puzzleRest = case zerosL+zerosR of
									1	-> if null filtered 
											then [] 
											else filter (\l -> l == (sumL+sumR)) [last filtered]
									_	-> okPermute filtered (zerosL+zerosR) (sumL+sumR)
	where
		filtered = filter (<=sumL+sumR) $ intersect valsL valsR
		(sumL,zerosL,valsL) = rowValuesDone puzzleDone 0 0 posVal
		(sumR,zerosR,valsR) = rowValuesLeft puzzleRest 0 0 posVal
	

rowValuesDone :: KakuroHalf -> Int -> Int -> [Int] -> (Int,Int,[Int])
rowValuesDone [] sumv zeros vals = (sumv,zeros,vals)
rowValuesDone _ sumv zeros [] = (sumv,zeros,[])
rowValuesDone ((a,b,c):xs) sumv zeros vals =  case a of
										-2 	-> (sumv,zeros,vals)
										-1 	-> (sumv+b,zeros,vals)
										0	-> rowValuesDone xs sumv (zeros+1) vals
										_	-> rowValuesDone xs (sumv-a) zeros $ filter (/=a) vals
								
rowValuesLeft :: KakuroHalf -> Int -> Int -> [Int] -> (Int,Int,[Int])
rowValuesLeft [] sumv zeros vals = (sumv,zeros,vals)
rowValuesLeft _ sumv zeros [] = (sumv,zeros,[])
rowValuesLeft ((a,b,c):xs) sumv zeros vals = case a of
										-2	-> (sumv,zeros,vals)
										-1	-> (sumv,zeros,vals)
										0	-> rowValuesLeft xs sumv (zeros+1) vals
										_	-> rowValuesLeft xs (sumv-a) zeros $ filter (/=a) vals
			
-- FOR COLUMN								
columnValues :: KakuroHalf -> KakuroHalf -> Int -> [Int]
columnValues puzzleDone puzzleRest puzzleSide = case zerosL+zerosR of
										1	-> if null filtered 
												then [] 
												else filter (\l -> l == (sumL+sumR))  [last filtered]
										_	-> okPermute filtered (zerosL+zerosR) (sumL+sumR)
		where
			filtered = filter (<=sumL+sumR) $ intersect valsL valsR
			(sumL,zerosL,valsL) = columnValuesDone puzzleDone puzzleSide 0 0 posVal
			(sumR,zerosR,valsR) = columnValuesLeft puzzleRest puzzleSide 0 0 posVal
								
columnValuesDone :: KakuroHalf -> Int -> Int -> Int -> [Int] -> (Int,Int,[Int])
columnValuesDone puzzleDone puzzleSide sumv zeros vals = columnValuesDone' (drop (puzzleSide-1) puzzleDone) puzzleSide sumv zeros posVal

columnValuesDone' :: KakuroHalf -> Int -> Int -> Int -> [Int] -> (Int,Int,[Int])
columnValuesDone' [] _ sumv zeros vals = (sumv,zeros,vals)
columnValuesDone' _ _ sumv zeros [] = (sumv,zeros,[])
columnValuesDone' ((a,_,c):xs) puzzleSide sumv zeros vals = case a of
																-2	-> (sumv,zeros,vals)
																-1	-> (sumv+c,zeros,vals)
																0	-> columnValuesDone' (drop (puzzleSide-1) xs) puzzleSide sumv (zeros+1) vals
																_	-> columnValuesDone' (drop (puzzleSide-1) xs) puzzleSide (sumv-a) zeros $ filter (/=a) vals


columnValuesLeft :: KakuroHalf -> Int -> Int -> Int -> [Int] -> (Int,Int,[Int])
columnValuesLeft [] _ sumv zeros vals = (sumv,zeros,vals)
columnValuesLeft _ _ sumv zeros [] = (sumv,zeros,[])
columnValuesLeft ((a,b,c):xs) puzzleSide sumv zeros vals = case a of
															-2	-> (sumv,zeros,vals)
															-1	-> (sumv,zeros,vals)
															0	-> columnValuesLeft (drop (puzzleSide-1) xs) puzzleSide sumv (zeros+1) vals
															_	-> columnValuesLeft (drop (puzzleSide-1) xs) puzzleSide (sumv-a) zeros $ filter (/=a) vals

isqrt :: (Integral a) => a -> a
isqrt = floor . sqrt . fromIntegral

-- INPUT HELP STRING
inputhelp = "A correct <kakuro file> is structured as follows:\t[(-2,0,0),(-1,6,0), ... ,(9,0,0)]\nIn which every (value,horizontal,vertical) represents a tile/cell of the puzzle. \n <value> can be\n\t-2\t-> the tile is not part of the puzzle\n\t-1\t-> the tile is part of the puzzle problem and contains one or more sums (see horizontal and vertical)\n\t0\t-> the tile still has to be filled in\n\t[1..9]\t-> the tile has received a value\n <horizontal> holds the sum the following tiles/cells in the row need to achieve\n <vertical> holds the sum the following tiles/cells in the column need to achieve"