-- You can benchmark your code quickly and effectively with Criterion. See its
-- website for help: <http://www.serpentine.com/criterion/>.
import Criterion.Main
import Data.Char
import qualified Data.Vector as V
import Sudoku

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

ex6 :: Sudoku V.Vector Int -- "evil"
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

ex8 :: Sudoku V.Vector DigSet -- 17 clues
ex8 = intToDSSudoku $ readSudoku
  "........1\n\
  \.......23\n\
  \..4..5...\n\
  \...1.....\n\
  \....3.6..\n\
  \..7...58.\n\
  \....67...\n\
  \.1...4...\n\
  \52......."


readSudoku :: String -> Sudoku V.Vector Int
readSudoku s = Sudoku.fromList 3 $ concatMap (map parseChar) $ lines s
  where
    parseChar '.' = 0
    parseChar d | isDigit d = digitToInt d
                | otherwise = error ("readSudoku: invalid charcter " ++ show d)

eval = (! mkCellIdx 0 0)

main :: IO ()
main = defaultMain 
--  [ bench "backtrack ex1"  (whnf (eval . backtracking) ex1')
--  , bench "backtrack ex5"  (whnf (eval . backtracking) ex5')
--  , bench "backtrack2 ex1" (whnf (eval . backtracking2) ex1')
--  , bench "backtrack2 ex5" (whnf (eval . backtracking2) ex5')
--  , bench "backtrack3 ex1" (whnf (eval . backtracking3) ex1')
  [ bench "backtrack3 ex5" (whnf (eval . backtracking3) ex5')
  , bench "backtrack3 ex6" (whnf (eval . backtracking3) ex6')
  , bench "backtrack3 ex7" (whnf (eval . backtracking3) ex7')
  , bench "backtrack3 ex8" (whnf (eval . backtracking3) ex8)
  , bench "backtrack4 ex5" (whnf (eval . backtracking4) ex5')
  , bench "backtrack4 ex6" (whnf (eval . backtracking4) ex6')
  , bench "backtrack4 ex7" (whnf (eval . backtracking4) ex7')
  , bench "backtrack4 ex8" (whnf (eval . backtracking4) ex8)
  ]
