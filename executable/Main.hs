{-# OPTIONS_GHC -Wno-unused-top-binds -Wno-missing-signatures #-}
import Sudoku
import Data.Char
import qualified Data.Vector as V
import System.CPUTime
import Control.Exception.Base
import Control.Monad
import System.Environment
import System.Random


ex1 :: Sudoku V.Vector Int
ex1 =  readSudoku
  "36..712..\n\
  \.5....18.\n\
  \..92.47..\n\
  \....13.28\n\
  \4..5.2..9\n\
  \27.46....\n\
  \..53.89..\n\
  \.83....6.\n\
  \..769..43"

ex1' = intToDSSudoku ex1

ex2 :: Sudoku V.Vector Int
ex2 = readSudoku
  "53..7....\n\
  \6..195...\n\
  \.98....6.\n\
  \8...6...3\n\
  \4..8.3..1\n\
  \7...2...6\n\
  \.6....28.\n\
  \...419..5\n\
  \....8..79"

ex2' = intToDSSudoku ex2

ex3 :: Sudoku V.Vector Int --hard
ex3 = readSudoku
  "....18..3\n\
  \1.3...95.\n\
  \..4.5.81.\n\
  \..157..6.\n\
  \....3....\n\
  \.2..867..\n\
  \.48.9.5..\n\
  \.79...1.8\n\
  \5..84...."

ex3' = intToDSSudoku ex3

ex4 :: Sudoku V.Vector Int --medium
ex4 = readSudoku
  ".2..8.49.\n\
  \.5..4....\n\
  \...3.72..\n\
  \8...739..\n\
  \54..9..12\n\
  \..952...3\n\
  \..64.2...\n\
  \....3..4.\n\
  \.95.6..2."

ex4' = intToDSSudoku ex4

ex5 :: Sudoku V.Vector Int -- "evil"
ex5 = readSudoku
  "..4..3.2.\n\
  \..8..2...\n\
  \3.9...6.1\n\
  \.1..4...8\n\
  \...3.8...\n\
  \6...7..4.\n\
  \5.1...9.6\n\
  \...5..4..\n\
  \.4.9..3.."

ex5' = intToDSSudoku ex5

ex6 :: Sudoku V.Vector Int -- bad for backtracking
ex6 = readSudoku
  ".........\n\
  \.....3.85\n\
  \..1.2....\n\
  \...5.7...\n\
  \..4...1..\n\
  \.9.......\n\
  \5......73\n\
  \..2.1....\n\
  \....4...9"

ex6' = intToDSSudoku ex6

ex7 :: Sudoku V.Vector Int -- 17 clues
ex7 = readSudoku
  ".......1.\n\
  \.....2..3\n\
  \...4.....\n\
  \......5..\n\
  \4.16.....\n\
  \..71.....\n\
  \.5....2..\n\
  \....8..4.\n\
  \.3.91...."

ex7' = intToDSSudoku ex7

exLocked :: Sudoku V.Vector DigSet
exLocked = groupElimination $ intToDSSudoku $ readSudoku
  "984......\n\
  \..25...4.\n\
  \..19.4..2\n\
  \..6.9723.\n\
  \..36.2...\n\
  \2.9.3561.\n\
  \195768423\n\
  \427351896\n\
  \638..9751"

exLocked2 :: Sudoku V.Vector DigSet
exLocked2 = groupElimination $ intToDSSudoku $ readSudoku
  "34...6.7.\n\
  \.8....93.\n\
  \..2.3..6.\n\
  \....1....\n\
  \.9736485.\n\
  \.....2...\n\
  \.........\n\
  \...6.8.9.\n\
  \...923785"

exPairs :: Sudoku V.Vector DigSet
exPairs = groupElimination $ intToDSSudoku $ readSudoku
  "....6....\n\
  \....42736\n\
  \..673..4.\n\
  \.94....68\n\
  \....964.7\n\
  \6.7.5.923\n\
  \1......85\n\
  \.6..8.271\n\
  \..5.1..94"

ex8' :: Sudoku V.Vector DigSet
ex8' = repeatedElimination $ intToDSSudoku $ readSudoku ".28..7....16.83.7.....2.85113729.......73........463.729..7.......86.14....3..7.."

ex9' :: Sudoku V.Vector DigSet
ex9' = repeatedElimination $ intToDSSudoku $ readSudoku "..2193........7...7...4..198.3...6...45...23...7...5.437..8...6...6........5341.."

ex1'4 :: Sudoku V.Vector DigSet
ex1'4 = groupElimination $ readSudokuN 4 
  ".9...3.2.d..a...\n\
  \.....c....b..48.\n\
  \....1......29...\n\
  \..e.b...c.9...5d\n\
  \ge.3fd...b4....2\n\
  \.49..1...c5a..3.\n\
  \c1....a.dg....6.\n\
  \...a..451.....d9\n\
  \7...c4..........\n\
  \...c..5....76...\n\
  \.......1.8d.5c7.\n\
  \.f.d....6....1.4\n\
  \6a2.............\n\
  \......e.....d..8\n\
  \................\n\
  \8...d5.....b7ag."

ex2'4 :: Sudoku V.Vector DigSet
ex2'4 = groupElimination $ readSudokuN 4 
  ".....a.8.7.3....\n\
  \ac9.....2..6.8.3\n\
  \3.d.5.c......2..\n\
  \2.......a....e6.\n\
  \.......6......5.\n\
  \..ce.2..........\n\
  \.5.gc.....6a87b9\n\
  \f..87.e.4.2.c1.g\n\
  \....8.....32..1.\n\
  \6.......b..1f...\n\
  \...3...a.......b\n\
  \d.....2cg.......\n\
  \..........g.....\n\
  \8g.a.1.39.c....7\n\
  \...c..7....8.a..\n\
  \.93..8.g......c."

ex3'4 :: Sudoku V.Vector DigSet -- 20s for backtracking4
ex3'4 = groupElimination $ readSudokuN 4 
  "a........df7....\n\
  \g..c.....b..6..7\n\
  \......d....e9..g\n\
  \f.3..c4.g56....2\n\
  \47g.da...1......\n\
  \c..25...a9e....b\n\
  \....c6b..375....\n\
  \.....1.72...ca..\n\
  \dg4....b........\n\
  \e.71...8...6...4\n\
  \...3.45.dea.....\n\
  \..9.a...........\n\
  \.........4.....1\n\
  \..a..b..e......8\n\
  \................\n\
  \.f....g........."

ex5'4 :: Sudoku V.Vector DigSet -- 480s/500s for 4/3
ex5'4 = groupElimination $ readSudokuN 4 
  ".....db.........\n\
  \.9..1..2..a....e\n\
  \c.65....2..8..b.\n\
  \.3e...c..b..4...\n\
  \.2f..1.c..b...e.\n\
  \..3.4..f.1......\n\
  \.c.9.3.......b..\n\
  \..b.....c4..f..8\n\
  \.6.2.59.7...8...\n\
  \........f..3...5\n\
  \db..e6.74...c.21\n\
  \....3...bgd.9e.6\n\
  \9...........e...\n\
  \.4....5..e....7.\n\
  \.....e79...4...a\n\
  \.......8..7..f.."

ex6'4 :: Sudoku V.Vector DigSet -- 1263s/1460s for 4/3
ex6'4 = groupElimination $ readSudokuN 4 
  ".....1ag....6...\n\
  \1..4.2..........\n\
  \a9.g..6........8\n\
  \...6f.....c.....\n\
  \...f...2...3...c\n\
  \.......8c.f....4\n\
  \....9....2....1.\n\
  \...36..fdg......\n\
  \6........9.e....\n\
  \g.8a..f.36......\n\
  \fcd.....8.......\n\
  \....a6.b.....8..\n\
  \..a8e...1...g...\n\
  \.....f..7.....8.\n\
  \.e....8.....b.d.\n\
  \..6c53...8....f."

ex7'4 :: Sudoku V.Vector DigSet -- >24h, still not done
ex7'4 = groupElimination $ readSudokuN 4 
  "......b8......a.\n\
  \...7....gc.526..\n\
  \..6..........gb.\n\
  \..8.........75.c\n\
  \..35b1.2.e.7.9.6\n\
  \.d.9...fb4...ac.\n\
  \e......c....f...\n\
  \...f7..ea....24.\n\
  \.1.d.......2a.f5\n\
  \....2d.1...c....\n\
  \.....6.....9.b2.\n\
  \...b.5.....a.4..\n\
  \......g..58.c.6.\n\
  \...8..d.........\n\
  \..41a8..6.c.bf..\n\
  \....f...9......."

readSudoku :: String -> Sudoku V.Vector Int
readSudoku s = Sudoku.fromList 3 $ concatMap (map parseChar) $ lines s
  where
    parseChar '.' = 0
    parseChar d | isDigit d = digitToInt d
                | otherwise = error ("readSudoku: invalid charcter " ++ show d)

symbolToInt :: Char -> Int
symbolToInt c | c >= '0' && c <= '9' = digitToInt c
              | c >= 'a' && c <= 'z' = 10 + ord c - ord 'a'
              | c >= 'A' && c <= 'Z' = 10 + ord c - ord 'A'
              | otherwise            = error "symbolToInt: invalid character"

intToSymbol :: Int -> Char
intToSymbol i | i <= 9    = intToDigit i
              | otherwise = chr $ ord 'a' + (i - 10)

readSudokuN :: Int -> String -> Sudoku V.Vector DigSet
readSudokuN n s = Sudoku.fromList n (concatMap (map parseChar) $ lines s)
  where
    parseChar :: Char -> DigSet
    parseChar '.' = allDigits n
    parseChar d   = singletonDS $ symbolToInt d

showSudoku :: Sudoku V.Vector DigSet -> String
showSudoku s = unlines $ map (V.toList . (V.map dsToChar)) $ rows s
  where
    n = Sudoku.size s
    n2 = Sudoku.groupSize s
    dsToChar ds | isSingleton ds = intToSymbol (head (digits ds))
                | otherwise      = '.'


printLines :: Show a => Sudoku V.Vector a -> IO ()
printLines s = mapM_ (\row -> print row) $ rows s


timeComp :: (a -> b) -> a -> IO (Double, b)
timeComp f a = do
  start <- getCPUTime
  v <- evaluate $ f a
  end <- getCPUTime
  return (fromIntegral (end - start) / (10^12), v)

main :: IO ()
main = defaultMain

usage :: IO ()
usage = do
  putStrLn "g  <n> <c> -> generate sudoku"
  putStrLn "gs <n> <c> -> generate and solve sudoku"
  putStrLn "rs <n>     -> read and solve"

defaultMain :: IO ()
defaultMain = do
  args <- getArgs
  case args of
    ["g",nArg,cArg] -> generateSudoku (read nArg) (read cArg) >>= putStrLn . showSudoku
    ["gs",nArg,cArg] -> do
      sud <- generateSudoku (read nArg) (read cArg)
      putStrLn $ showSudoku sud
      andSolve sud
    ["rs",nArg]        -> readAndSolve (read nArg)
    _                 -> usage

generateSudoku :: Int -> Int -> IO (Sudoku V.Vector DigSet)
generateSudoku n c = do
  g1 <- newStdGen
  g2 <- newStdGen
  return $ removeRandom c g2 $ randomCompleted n g1

benchmarks :: IO ()
benchmarks = do
  g1 <- newStdGen
  g2 <- newStdGen
  let sud = removeRandom 190 g2 $ randomCompleted 4 g1
  printLines sud
  (t1,sol) <- timeComp backtracking3 sud
  printLines sol
  putStrLn ("valid: " ++ show (validDS sol))
  putStrLn ("time: " ++ show t1 ++ " s")
  (t2,sol2) <- timeComp backtracking4 sud
  printLines sol2
  putStrLn ("valid: " ++ show (validDS sol2))
  putStrLn ("time: " ++ show t2 ++ " s")
  --(t1,sol) <- timeComp backtracking3 ex5'
  --printLines sol
  --putStrLn ("valid: " ++ show (validDS sol))
  --putStrLn ("time: " ++ show t1 ++ " s")
  --(t2,sol2) <- timeComp backtracking3 ex7'
  --printLines sol2
  --putStrLn ("valid: " ++ show (validDS sol2))
  --putStrLn ("time: " ++ show t2 ++ " s")
  --(t3,sol3) <- timeComp backtracking3 ex6'
  --printLines sol3
  --putStrLn ("valid: " ++ show (validDS sol3))
  --putStrLn ("time: " ++ show t3 ++ " s")

benchmarks2 :: IO ()
benchmarks2 = do
  return ()
  --(t1,sol) <- timeComp backtracking4 ex5'
  --printLines sol
  --putStrLn ("valid: " ++ show (validDS sol))
  --putStrLn ("time: " ++ show t1 ++ " s")
  ----(t4,res) <- timeComp hiddenPairs (groupElimination ex7')
  ----printLines res
  ----putStrLn ("valid: " ++ show (validDS res))
  ----putStrLn ("time: " ++ show t4 ++ " s")
  --(t2,sol2) <- timeComp backtracking4 ex7'
  --printLines sol2
  --putStrLn ("valid: " ++ show (validDS sol2))
  --putStrLn ("time: " ++ show t2 ++ " s")
  --(t3,sol3) <- timeComp backtracking4 ex6'
  --printLines sol3
  --putStrLn ("valid: " ++ show (validDS sol3))
  --putStrLn ("time: " ++ show t3 ++ " s")

readAndSolve :: Int -> IO ()
readAndSolve n = readSudokuIO n >>= andSolve

readSudokuIO :: Int -> IO (Sudoku V.Vector DigSet)
readSudokuIO n = readSudokuN n <$> concat <$> replicateM (n*n) getLine

andSolve :: Sudoku V.Vector DigSet -> IO ()
andSolve sudoku = do
  _ <- evaluate sudoku
  putStrLn "solving using `backtacking4`..."
  (t1,sol1) <- timeComp backtracking4 sudoku
  putStrLn $ showSudoku sol1
  putStrLn ("time: " ++ show t1 ++ " s")
  putStrLn "solving using `backtacking3`..."
  (t2,sol2) <- timeComp backtracking3 sudoku
  putStrLn $ showSudoku sol2
  putStrLn ("time: " ++ show t2 ++ " s")
  case sol1 == sol2 of
    True  -> putStrLn "EQ"
    False -> putStrLn "NEQ"
