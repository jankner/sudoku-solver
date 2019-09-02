-- | Sudoku
-- {-# LANGUAGE TypeInType, TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MagicHash #-}
module Sudoku
  ( Sudoku
  , DigSet
  , (!)
  , rowIdxsAt, colIdxsAt, sqrIdxsAt
  , rowIdxs
  , sqrIdxs, sqrRowSpan, sqrColSpan
  , rows , cols , sqrs
  , rowAt , colAt , sqrAt
  , empty
  , size
  , groupSize
  , cellCount
  , fromList
  , mapSudoku
  , imapSudoku
  , valid
  , validDS
  , groupValid
  , mkCellIdx
  , flatIndex
  , fromFlatIndex
  , digits
  , decomposeDS
  , allDigits
  , emptyDS
  , singletonDS
  , isSingleton
  , fromDigits
  , intersectDS
  , unionDS
  , diffDS
  , overlapsDS
  , groupDS
  , remainingDS
  , intToDSSudoku
  , dsToIntSudoku
  , hiddenSingles
  , hiddenPairs
  , findGroupHiddenSingles
  , findGroupHiddenPairs
  , lockedCandidates1
  , findLockedCandidate1
  , groupElimination
  , repeatedElimination
  , checkGuess
  , checkGroupDS
  , backtracking
  , backtracking2
  , backtracking3
  , backtracking4
  , advanced1
  , randomCompleted
  , removeRandom
  ) where

import Control.Arrow
import Control.Monad.Writer
import Control.Monad.ST
import Data.Vector (Vector)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import Data.Set (Set)
import qualified Data.Set as DS
import Data.Bits
import Data.Word
import Data.Foldable
import Data.List
import Math.NumberTheory.Logarithms

import System.Random
import System.Random.Shuffle

import Debug.Trace
--trace = const id
--traceShow = const id
--traceShowId = id
traceShowName name a = trace ((showString name . showString ": " . shows a) "")
traceShowIdName name a = trace ((showString name . showString ": " . shows a) "") a

data CellIdx = CellIdx {-# UNPACK #-} !Word16 {-# UNPACK #-} !Word16
  deriving (Eq,Ord)

instance Show CellIdx where
  showsPrec d (CellIdx r c) = showsPrec d (r, c)

{-# INLINE mkCellIdx #-}
mkCellIdx :: Integral a => a -> a -> CellIdx
mkCellIdx r c = CellIdx (fromIntegral r) (fromIntegral c)

{-# INLINE row #-}
row :: CellIdx -> Int
row (CellIdx r c) = fromIntegral r

{-# INLINE col #-}
col :: CellIdx -> Int
col (CellIdx r c) = fromIntegral c

{-# INLINE sqr #-}
sqr :: Int -> CellIdx -> Int
sqr n (CellIdx r c) = fromIntegral $ n * (r' `div` n) + c' `div` n
  where
    r' = fromIntegral r
    c' = fromIntegral c

sqrRowSpan :: Int -> Int -> (Int, Int)
sqrRowSpan n sqr = let b = n * (sqr `div` n) in (b, b + n)

sqrColSpan :: Int -> Int -> (Int, Int)
sqrColSpan n sqr = let b = n * (sqr `mod` n) in (b, b + n)

{-# INLINE flatIndex #-}
flatIndex :: Int -> CellIdx -> Int
flatIndex n idx = n*n*(row idx) + (col idx)

{-# INLINE fromFlatIndex #-}
fromFlatIndex :: Int -> Int -> CellIdx
fromFlatIndex n idx = CellIdx (fromIntegral (fromIntegral idx `div` n2)) (fromIntegral (fromIntegral idx `mod` n2))
  where
    n2 = n*n

{-# INLINE zeroIdx #-}
zeroIdx :: CellIdx
zeroIdx = CellIdx 0 0

{-# INLINE nextIdx #-}
nextIdx :: Int -> CellIdx -> CellIdx
nextIdx n (CellIdx r c)
  | (fromIntegral c)+1 < n2  = CellIdx r (c+1)
  | otherwise                = CellIdx (r+1) 0
  where
    n2 = n*n

{-# INLINE isLastIdx #-}
isLastIdx :: Int -> CellIdx -> Bool
isLastIdx n (CellIdx r c) = r == fromIntegral (n*n-1) && c == fromIntegral (n*n-1)

{-# INLINE isPastLastIdx #-}
isPastLastIdx :: Int -> CellIdx -> Bool
isPastLastIdx n (CellIdx r c) = r >= fromIntegral (n*n) || c >= fromIntegral (n*n)

newtype DigSet = DigSet Word64
  deriving (Eq,Ord,Bits)

isSingleton :: DigSet -> Bool
isSingleton (DigSet i) = popCount i == 1

fromSingletonDigSet :: DigSet -> Int
fromSingletonDigSet (DigSet i) = integerLog2' (fromIntegral i) + 1

instance Show DigSet where
  showsPrec d (DigSet i) | popCount i == 1 = showString " " . shows (fromSingletonDigSet (DigSet i))
                         | otherwise       = showString "?" . shows (popCount i)

digits :: DigSet -> [Int]
digits (DigSet i) = go i 1
  where
    go 0 d               = []
    go i d | testBit i 0 = d : go (i `shiftR` 1) (d + 1)
           | otherwise   = go (i `shiftR` 1) (d + 1) 

decomposeDS :: DigSet -> [DigSet]
decomposeDS (DigSet i) = go i 1
  where
    go 0 d               = []
    go i d | testBit i 0 = singletonDS d : go (i `shiftR` 1) (d + 1)
           | otherwise   = go (i `shiftR` 1) (d + 1) 

fromDigits :: [Int] -> DigSet
fromDigits xs = foldr' (\x ds -> ds `unionDS` singletonDS x) emptyDS xs

allDigits :: Int -> DigSet
allDigits n = DigSet (2^(n*n) - 1)

emptyDS :: DigSet
emptyDS = DigSet 0

isEmptyDS :: DigSet -> Bool
isEmptyDS (DigSet 0) = True
isEmptyDS _          = False

singletonDS :: Integral i => i -> DigSet
singletonDS i = bit (fromIntegral (i-1))

fromDigitDS :: Int -> DigSet
fromDigitDS 0 = emptyDS
fromDigitDS i = singletonDS i

intersectDS :: DigSet -> DigSet -> DigSet
intersectDS x y = x .&. y

unionDS :: DigSet -> DigSet -> DigSet
unionDS x y = x .|. y

diffDS :: DigSet -> DigSet -> DigSet
diffDS (DigSet x) (DigSet y) = DigSet $ x .&. (complement y)

overlapsDS :: DigSet -> DigSet -> Bool
overlapsDS x y = not (isEmptyDS (x .&. y))

--diffDS :: DigSet -> DigSet -> DigSet
--diffDS (DigSet x) (DigSet y) = DigSet $ x .&. (y `xor` mask)
--  where
--    mask = (1 `shiftL` (highestSetBit + 1)) - 1
--    highestSetBit = max (integerLog2' x + 1) (integerLog2' y + 1)

data Sudoku v a = Sudoku Int (v a)
  deriving (Eq,Ord,Show)

data Group
  = Row Int
  | Col Int
  | Sqr Int
  deriving (Eq,Ord,Show)

emptySudoku :: G.Vector v DigSet => Int -> Sudoku v DigSet
emptySudoku n = Sudoku n (G.replicate (n^4) (allDigits n))

type Changes = Set Group

affectedIndices :: [CellIdx] -> [CellIdx]
affectedIndices cs = undefined

{-# INLINE rowIdxs #-}
rowIdxs :: Int -> Vector (Vector CellIdx)
rowIdxs n = G.generate (n * n) (\r -> G.generate (n * n) (\j -> mkCellIdx r j))

{-# INLINE colIdxs #-}
colIdxs :: Int -> Vector (Vector CellIdx)
colIdxs n = G.generate (n * n) (\c -> G.generate (n * n) (\j -> mkCellIdx j c))

{-# INLINE sqrIdxs #-}
sqrIdxs :: Int -> Vector (Vector CellIdx)
sqrIdxs n = G.generate (n * n) (\i -> G.generate (n * n) (\j -> mkCellIdx (n * (i `div` n) + j `div` n) (n * (i `mod` n) + j `mod` n)))

{-# INLINE rowIdxsAt #-}
rowIdxsAt :: Int -> CellIdx -> Vector CellIdx
rowIdxsAt n idx = G.generate (n * n) (\j -> mkCellIdx (row idx) j)

{-# INLINE colIdxsAt #-}
colIdxsAt :: Int -> CellIdx -> Vector CellIdx
colIdxsAt n idx = G.generate (n * n) (\j -> mkCellIdx j (col idx))

{-# INLINE sqrIdxsAt #-}
sqrIdxsAt :: Int -> CellIdx -> Vector CellIdx
sqrIdxsAt n idx = G.generate (n * n) (\j -> mkCellIdx (n * r + j `div` n) (n * c + j `mod` n))
  where
    r = row idx `div` n
    c = col idx `div` n

size :: Sudoku v a -> Int
size (Sudoku n _) = n

groupSize :: Sudoku v a -> Int
groupSize (Sudoku n _) = n^2

cellCount :: Sudoku v a -> Int
cellCount (Sudoku n _) = n^4

-- | create an empty Sudoku. that is, a Sudoku where all cells are 0
empty
  :: G.Vector v Integer
  => Int -- ^ size `n` of the sudoku. number of rows and columns is `n^2`. number of cells is `n^4`
  -> Sudoku v Integer
empty n = Sudoku n (G.replicate (n^4) 0)

fromList
  :: G.Vector v a
  => Int -- * size of sudoku
  -> [a]
  -> Sudoku v a
fromList n xs = if (n^4 == G.length v)
                  then Sudoku n v
                  else error "fromList: ..."
  where
    v = G.fromList xs

mapSudoku :: (G.Vector v a, G.Vector v b) => (a -> b) -> Sudoku v a -> Sudoku v b
mapSudoku f (Sudoku n v) = Sudoku n (G.map f v)

imapSudoku :: (G.Vector v a, G.Vector v b) => (CellIdx -> a -> b) -> Sudoku v a -> Sudoku v b
imapSudoku f (Sudoku n v) = Sudoku n (G.imap (wrap f) v)
  where
    wrap f = f . fromFlatIndex n

convertVectorSudoku :: (G.Vector v a, G.Vector w a) => Sudoku v a -> Sudoku w a
convertVectorSudoku (Sudoku n v) = Sudoku n (G.convert v)

(!) :: G.Vector v a => Sudoku v a -> CellIdx -> a
(Sudoku n v) ! idx = v G.! flatIndex n idx

rowN :: G.Vector v a => Sudoku v a -> Int -> v a
rowN (Sudoku n v) i = G.slice (n*n*i) (n*n) v

rows :: G.Vector v a => Sudoku v a -> [v a]
rows s = map (rowN s) [0..groupSize s - 1]

rowsV :: (G.Vector w (v a), G.Vector v a) => Sudoku v a -> w (v a)
rowsV s = G.generate (groupSize s) (rowN s)

rowAt :: G.Vector v a => Sudoku v a -> CellIdx -> v a
rowAt s idx = rowN s (row idx)

colN :: G.Vector v a => Sudoku v a -> Int -> v a
colN s@(Sudoku _ v) i = G.generate (groupSize s) (\j -> s ! mkCellIdx j i)

cols :: G.Vector v a => Sudoku v a -> [v a]
cols s = map (colN s) [0..groupSize s - 1]

colsV :: (G.Vector w (v a), G.Vector v a) => Sudoku v a -> w (v a)
colsV s = G.generate (groupSize s) (colN s)

colAt :: G.Vector v a => Sudoku v a -> CellIdx -> v a
colAt s idx = colN s (col idx)

sqrN :: G.Vector v a => Sudoku v a -> Int -> v a
sqrN s@(Sudoku n v) i = G.generate (groupSize s) (\j -> s ! mkCellIdx (n * idivn + j `div` n) (n * imodn + j `mod` n))
  where
    idivn = i `div` n
    imodn = i `mod` n

sqrRC :: G.Vector v a => Sudoku v a -> Int -> Int -> v a
sqrRC s@(Sudoku n v) r c = G.generate (groupSize s) (\j -> s ! mkCellIdx (n * r + j `div` n) (n * c + j `mod` n))

sqrs :: G.Vector v a => Sudoku v a -> [v a]
sqrs s = sqrRC s <$> [0..size s - 1] <*> [0..size s - 1]

sqrsV :: (G.Vector w (v a), G.Vector v a) => Sudoku v a -> w (v a)
sqrsV s = G.generate (groupSize s) (sqrN s)

sqrAt :: G.Vector v a => Sudoku v a -> CellIdx -> v a
sqrAt s@(Sudoku n v) idx = sqrRC s (row idx `div` n) (col idx `div` n)


dsToIntSudoku
  :: (G.Vector v Int , G.Vector v DigSet)
  => Sudoku v DigSet
  -> Sudoku v Int
dsToIntSudoku s = mapSudoku cc s 
  where
    cc ds | isSingleton ds = fromSingletonDigSet ds
          | otherwise      = 0

valid :: G.Vector v Int => Sudoku v Int -> Bool
valid s = all (checkGroup emptyDS) (rows s) && all (checkGroup emptyDS) (cols s) && all (checkGroup emptyDS) (sqrs s)

validDS :: G.Vector v DigSet => Sudoku v DigSet -> Bool
validDS s = all (checkGroupDS emptyDS) (rows s) && all (checkGroupDS emptyDS) (cols s) && all (checkGroupDS emptyDS) (sqrs s)

solvable :: G.Vector v DigSet => Sudoku v DigSet -> Bool 
solvable (Sudoku n v) = G.all (/= emptyDS) v

solved :: G.Vector v Int => Sudoku v Int -> Bool
solved (Sudoku n v) = G.all (/=0) v

solvedDS :: G.Vector v DigSet => Sudoku v DigSet -> Bool
solvedDS (Sudoku n v) = G.all isSingleton v

groupValid :: G.Vector v Int => v Int -> Bool 
groupValid v = checkGroup emptyDS v

checkGroup :: G.Vector v Int => DigSet -> v Int -> Bool
checkGroup s g = go s (G.toList g)
  where
    go :: DigSet -> [Int] -> Bool
    go s []     = True
    go s (0:xs) = go s xs
    go s (x:xs) = not (s `testBit` x) && go (s `setBit` x) xs

checkGroupDS :: G.Vector v DigSet => DigSet -> v DigSet -> Bool
checkGroupDS s g = go s (G.toList g)
  where
    go :: DigSet -> [DigSet] -> Bool
    go s []                     = True
    go s (x:xs) | isSingleton x = not (isEmptyDS (x `diffDS` s)) && go (s `unionDS` x) xs
                | otherwise     = go s xs


groupDS :: G.Vector v Int => v Int -> DigSet
groupDS g = G.foldr (\i ds -> ds `unionDS` (singletonDS i)) emptyDS g

remainingDS :: G.Vector v Int => Int -> v Int -> DigSet
remainingDS n g = allDigits n `diffDS` groupDS g

intToDSSudoku :: (G.Vector v Int, G.Vector v DigSet) => Sudoku v Int -> Sudoku v DigSet
intToDSSudoku s@(Sudoku n v) = Sudoku n (G.map (\case { 0 -> allDigits n; d -> singletonDS d }) v)

nakedPairs :: forall v. G.Vector v DigSet => Sudoku v DigSet -> Sudoku v DigSet
nakedPairs s@(Sudoku n v) = undefined

hiddenSingles :: forall v. G.Vector v DigSet => Sudoku v DigSet -> (Int, Sudoku v DigSet)
hiddenSingles s@(Sudoku n v) = (0, Sudoku n (G.modify writeSingles v))
  where
    writeIndices :: G.Mutable v s DigSet -> [(CellIdx,DigSet)] -> ST s ()
    writeIndices mv xs = mapM_ (\(idx, ds) -> GM.write mv (flatIndex n idx) ds) xs
    writeSingles :: G.Mutable v s DigSet -> ST s ()
    writeSingles mv = writeIndices mv rowSingles >> writeIndices mv colSingles >> writeIndices mv sqrSingles
    rowSingles = concat $ zipWith (\r -> map (\(c,ds) -> (mkCellIdx r c, ds))) [0..groupSize s - 1] (map findGroupHiddenSingles (rows s))
    colSingles = concat $ zipWith (\c -> map (\(r,ds) -> (mkCellIdx r c, ds))) [0..groupSize s - 1] (map findGroupHiddenSingles (cols s))
    sqrSingles = concat $ zipWith (\sqr -> map (\(i,ds) -> (sqrCoordToCellIdx sqr i, ds))) [0..groupSize s - 1] (map findGroupHiddenSingles (sqrs s))
    sqrCoordToCellIdx sqrNum sqrPos = mkCellIdx (n * (sqrNum `div` n) + sqrPos `div` n) (n * (sqrNum `mod` n) + sqrPos `mod` n)


findGroupHiddenSingles :: G.Vector v DigSet => v DigSet -> [(Int, DigSet)]
findGroupHiddenSingles g = let (once, twice, firsts) = G.ifoldr' f (emptyDS, emptyDS, []) g in filter (\(i,ds) -> not (ds `overlapsDS` twice)) firsts
  where
    f :: Int -> DigSet -> (DigSet, DigSet, [(Int,DigSet)]) -> (DigSet, DigSet, [(Int,DigSet)])
    f i ds (once,twice,firsts) = (once', twice', firsts' ++ firsts)
      where
        once'   = once `unionDS` ds
        twice'  = twice `unionDS` (once `intersectDS` ds) 
        firsts' = if isSingleton ds
                    then []
                    else map (\dig -> (i, singletonDS dig) ) $ digits $ ds `diffDS` once

hiddenPairs :: forall v. G.Vector v DigSet => Sudoku v DigSet -> Sudoku v DigSet
hiddenPairs s@(Sudoku n v) = Sudoku n (G.modify writePairs v)
  where
    writeElim :: G.Mutable v s DigSet -> (DigSet, CellIdx, CellIdx) -> ST s ()
    writeElim mv (ds, idx1, idx2) = do
      GM.modify mv (\ds' -> ds `intersectDS` ds') (flatIndex n idx1)
      GM.modify mv (\ds' -> ds `intersectDS` ds') (flatIndex n idx2)
    writePairs :: G.Mutable v s DigSet -> ST s ()
    writePairs mv = do
      G.mapM_ (mapM_ (writeElim mv)) rowPairs
      G.mapM_ (mapM_ (writeElim mv)) colPairs
      G.mapM_ (mapM_ (writeElim mv)) sqrPairs
    rowPairs = G.map (findGroupHiddenPairs s) (rowIdxs n)
    colPairs = G.map (findGroupHiddenPairs s) (colIdxs n)
    sqrPairs = G.map (findGroupHiddenPairs s) (sqrIdxs n)

combinations :: [a] -> [(a,a)]
combinations xs = do
  y1:xs' <- tails xs
  y2:_   <- tails xs'
  return (y1,y2)

findGroupHiddenPairs :: G.Vector v DigSet => Sudoku v DigSet -> Vector CellIdx -> [(DigSet,CellIdx,CellIdx)]
findGroupHiddenPairs s idxs = concat $ do
  c1:candidates' <- tails candidates
  c2:_           <- tails candidates'
  return $ f (c1 `unionDS` c2)
 where
   candidates = decomposeDS (G.foldr' unionDS emptyDS $ G.filter (not . isSingleton) (G.map (s !) idxs))
   f :: DigSet -> [(DigSet,CellIdx,CellIdx)]
   f ds =
     case G.toList (G.filter (g ds) idxs) of
       [idx1,idx2] -> [(ds, idx1, idx2)]
       _           -> []
   g ds idx = ds `overlapsDS` (s ! idx)

findGroupHiddenSets :: G.Vector v DigSet => Sudoku v DigSet -> Vector CellIdx -> [(DigSet,[CellIdx])]
findGroupHiddenSets s idxs = undefined
 where
--   f :: Int -> DigSet -> [(DigSet, [(DigSet,Int)])] -> [(DigSet, [(DigSet,Int)])]
--   f i ds []            = [(ds, [i])]
--   f i ds ((digs,_):xs) = undefined

lockedCandidates1 :: forall v. G.Vector v DigSet => Sudoku v DigSet -> Sudoku v DigSet
lockedCandidates1 s@(Sudoku n v) = Sudoku n $ G.modify (\mv -> sequence_ (findAndWrite mv <$> [0..8] <*> map singletonDS [1..9])) v
  where
    findAndWrite :: G.Mutable v s DigSet-> Int -> DigSet  -> ST s ()
    findAndWrite mv sqr d = eliminateFromCells mv d (findLockedCandidate1 s d sqr)
    eliminateFromCells :: G.Mutable v s DigSet -> DigSet -> Vector CellIdx -> ST s ()
    eliminateFromCells mv d idxs = if G.null idxs
                                     then G.mapM_ (\idx -> GM.modify mv (\ds -> ds `diffDS` d) (flatIndex n idx)) idxs
                                     else G.mapM_ (\idx -> GM.modify mv (\ds -> ds `diffDS` d) (flatIndex n idx)) idxs

findLockedCandidate1 :: G.Vector v DigSet => Sudoku v DigSet -> DigSet -> Int -> Vector CellIdx
findLockedCandidate1 s d sqr | G.length idxs < 2 = G.empty
                             | otherwise         =
                               case (allInRow, allInCol) of
                                 (Just r, _)       -> rowIdxsMinusSquare r
                                 (Nothing, Just c) -> colIdxsMinusSquare c
                                 _                 -> G.empty
  where
    idxs = G.filter (\idx -> (s ! idx) `overlapsDS` d) $ sqrIdxs (size s) G.! sqr
    allInRow = G.foldr1' eql (G.map (Just . row) idxs)
    allInCol = G.foldr1' eql (G.map (Just . col) idxs)
    eql :: Eq a => Maybe a -> Maybe a -> Maybe a
    eql Nothing  _                 = Nothing 
    eql (Just a) Nothing           = Nothing
    eql (Just a) (Just b) | a == b = Just b
                          | a /= b = Nothing
    (rowStart, colStart) = ((* size s) *** (* size s)) (sqr `divMod` size s)
    n = size s
    n2 = groupSize s
    (rowBeg, rowEnd) = sqrRowSpan n sqr
    (colBeg, colEnd) = sqrColSpan n sqr
    rowIdxsMinusSquare :: Int -> Vector CellIdx
    rowIdxsMinusSquare r = G.filter (\idx -> col idx < colBeg || col idx >= colEnd) (rowIdxs n G.! r)
    colIdxsMinusSquare :: Int -> Vector CellIdx
    colIdxsMinusSquare c = G.filter (\idx -> row idx < rowBeg || row idx >= rowEnd) (colIdxs n G.! c)
    -- rowIdxs r = G.generate (n2 - n) (\i -> mkCellIdx r (if i < colStart then i else i + size s))
    -- colIdxs c = G.generate (n2 - n) (\i -> mkCellIdx (if i < rowStart then i else i + size s) c)
    

groupEliminates :: forall v. G.Vector v DigSet => v DigSet -> DigSet
groupEliminates g = G.foldr unionDS emptyDS (G.filter isSingleton g)

groupElimination :: forall v. G.Vector v DigSet => Sudoku v DigSet -> Sudoku v DigSet
groupElimination s@(Sudoku n v) = imapSudoku eliminate s
  where
    rowEliminates :: Vector DigSet
    rowEliminates = G.map groupEliminates (rowsV s)
    colEliminates :: Vector DigSet
    colEliminates = G.map groupEliminates (colsV s)
    sqrEliminates :: Vector DigSet
    sqrEliminates = G.map groupEliminates (sqrsV s)
    eliminate :: CellIdx -> DigSet -> DigSet
    eliminate i ds | isSingleton ds = ds
                   | otherwise      = eliminateSqr i $ eliminateCol i $ eliminateRow i ds
    eliminateRow :: CellIdx -> DigSet -> DigSet
    eliminateRow i ds = ds `diffDS` (rowEliminates G.! row i)
    eliminateCol :: CellIdx -> DigSet -> DigSet
    eliminateCol i ds = ds `diffDS` (colEliminates G.! col i)
    eliminateSqr :: CellIdx -> DigSet -> DigSet
    eliminateSqr i ds = ds `diffDS` (sqrEliminates G.! sqr n i)

groupElimination' :: G.Vector v DigSet => Set CellIdx -> Sudoku v DigSet -> Sudoku v DigSet
groupElimination' changeSet s@(Sudoku n v) = undefined
  where
    eliminate :: CellIdx -> DigSet -> DigSet
    eliminate i ds | isSingleton ds = ds
                   | otherwise      = eliminateSqr i $ eliminateCol i $ eliminateRow i ds
    eliminateRow :: CellIdx -> DigSet -> DigSet
    eliminateRow i ds = ds `diffDS` (groupEliminates (rowAt s i))
    eliminateCol :: CellIdx -> DigSet -> DigSet
    eliminateCol i ds = ds `diffDS` (groupEliminates (colAt s i))
    eliminateSqr :: CellIdx -> DigSet -> DigSet
    eliminateSqr i ds = ds `diffDS` (groupEliminates (sqrAt s i))

repeatedElimination :: (Eq (v DigSet), G.Vector v DigSet) => Sudoku v DigSet -> Sudoku v DigSet
repeatedElimination s 
  | s == s'   = s
  | otherwise = repeatedElimination s'
  where
    s' = groupElimination s

backtracking :: forall v. (Show (v DigSet), G.Vector v DigSet) => Sudoku v DigSet -> Sudoku v DigSet
backtracking s = let sols = go zeroIdx s in {- trace (show (length sols)) $ -} head sols
  where
    go :: CellIdx -> Sudoku v DigSet -> [Sudoku v DigSet]
    go idx s@(Sudoku n v)
      | not (validDS s)       = error $ "invalid:\n" ++ unlines (map show (rows s))
      | isPastLastIdx n idx   = {- trace ("end" ++ show idx) $ -} [s]
      | isSingleton (s ! idx) = {- trace "singleton, next" -} go (nextIdx n idx) s
      | otherwise             = {- trace ("guessing at " ++ show idx) $ -} concatMap (go (nextIdx n idx)) $ map (mkGuess idx s) $ {- traceShowId $ -} filter (checkGuess idx s) (digits (s ! idx))
    mkGuess idx (Sudoku n v) guess = {- trace ("guess: " ++ show guess ++ " at " ++ show idx) $ -} Sudoku n (G.modify (writeGuessM (flatIndex n idx) (singletonDS guess)) v)
    writeGuessM idx guess v = GM.write v idx guess

backtracking2 :: forall v. (Show (v DigSet), Eq (v DigSet), G.Vector v DigSet) => Sudoku v DigSet -> Sudoku v DigSet
backtracking2 s = let sols = go zeroIdx s in head sols
  where
    go :: CellIdx -> Sudoku v DigSet -> [Sudoku v DigSet]
    go idx s@(Sudoku n v)
      | not (validDS s')       = []
      | not (solvable s')      = []
      | solvedDS s'            = [s']
      | isPastLastIdx n idx    = [s']
      | isSingleton (s' ! idx) = go (nextIdx n idx) s'
      | otherwise              = concatMap (go (nextIdx n idx)) $ map (mkGuess idx s') $ filter (checkGuess idx s') (digits (s' ! idx))
      where
        s' = repeatedElimination s
        
          -- let
        --  x = repeatedElimination s
        --      in case validDS x of
        --        False -> trace ("bad\n" ++ unlines (map show (rows x))) x
        --        True  -> trace ("good\n" ++ unlines (map show (rows x))) x
    mkGuess idx (Sudoku n v) guess = Sudoku n (G.modify (writeGuessM (flatIndex n idx) (singletonDS guess)) v)
    writeGuessM idx guess v = GM.write v idx guess

backtracking3 :: forall v. (Show (v DigSet), Eq (v DigSet), G.Vector v DigSet) => Sudoku v DigSet -> Sudoku v DigSet
backtracking3 s = let sols = go zeroIdx s in head sols
  where
    go :: CellIdx -> Sudoku v DigSet -> [Sudoku v DigSet]
    go idx s@(Sudoku n v)
      | not (validDS s')       = []
      | not (solvable s')      = []
      | solvedDS s'            = [s']
      | isPastLastIdx n idx    = [s']
      | isSingleton (s' ! idx) = go (nextIdx n idx) s'
      | otherwise              = concatMap (go (nextIdx n idx)) $ map (mkGuess idx s') $ filter (checkGuess idx s') (digits (s' ! idx))
      where
        s' = stuff s
        stuff su = let
          su1     = groupElimination su
          (i,su2) = hiddenSingles su1
                    in if su2 == su
                         then su2
                         else stuff su2
          -- let
        --  x = repeatedElimination s
        --      in case validDS x of
        --        False -> trace ("bad\n" ++ unlines (map show (rows x))) x
        --        True  -> trace ("good\n" ++ unlines (map show (rows x))) x
    mkGuess idx (Sudoku n v) guess = Sudoku n (G.modify (writeGuessM (flatIndex n idx) (singletonDS guess)) v)
    writeGuessM idx guess v = GM.write v idx guess

backtracking4 :: forall v. (Show (v DigSet), Eq (v DigSet), G.Vector v DigSet) => Sudoku v DigSet -> Sudoku v DigSet
backtracking4 s = let sols = go zeroIdx s in head sols
  where
    go :: CellIdx -> Sudoku v DigSet -> [Sudoku v DigSet]
    go idx s@(Sudoku n v)
      | not (validDS s')       = []
      | not (solvable s')      = []
      | solvedDS s'            = [s']
      | isPastLastIdx n idx    = [s']
      | isSingleton (s' ! idx) = go (nextIdx n idx) s'
      | otherwise              = concatMap (go (nextIdx n idx)) $ map (mkGuess idx s') $ filter (checkGuess idx s') (digits (s' ! idx))
      where
        s' = stuff s
        stuff su = let
          su1     = groupElimination su
          (i,su2) = hiddenSingles su1
          --su3     = lockedCandidates1 su2
                    in if su2 == su
                         then stuff2 su2
                         else stuff su2
        stuff2 su = let
          su1 = hiddenPairs su
          su2 = lockedCandidates1 su1
                     in if su2 == su
                          then su2
                          else stuff su2
          -- let
        --  x = repeatedElimination s
        --      in case validDS x of
        --        False -> trace ("bad\n" ++ unlines (map show (rows x))) x
        --        True  -> trace ("good\n" ++ unlines (map show (rows x))) x
    mkGuess idx (Sudoku n v) guess = Sudoku n (G.modify (writeGuessM (flatIndex n idx) (singletonDS guess)) v)
    writeGuessM idx guess v = GM.write v idx guess

data SolvStats = SolvStats
  { statNBeforeBacktrack :: Int
  , statNBacktrackGuesses :: Int
  }
  deriving (Eq,Show)

instance Monoid SolvStats where
  mempty = SolvStats 0 0
  a `mappend` b = SolvStats
    (statNBeforeBacktrack a + statNBeforeBacktrack b)
    (statNBacktrackGuesses a + statNBacktrackGuesses b)

newtype SolvM a = SolvM { unSolvM :: Writer SolvStats a }

runSolvM :: SolvM a -> a
runSolvM m = traceShow stats a
  where
    (a, stats) = runWriter (unSolvM m)

advanced1 :: Sudoku Vector DigSet -> [Sudoku Vector DigSet]
advanced1 s = runSolvM (go s)
  where
    go :: Sudoku Vector DigSet -> SolvM [Sudoku Vector DigSet]
    go s@(Sudoku n v) = undefined

checkGuess :: G.Vector v DigSet => CellIdx -> Sudoku v DigSet -> Int -> Bool
checkGuess idx s guess =
  checkGroupDS (singletonDS guess) (rowAt s idx) &&
  checkGroupDS (singletonDS guess) (colAt s idx) &&
  checkGroupDS (singletonDS guess) (sqrAt s idx)

randomCompleted :: forall g. RandomGen g => Int -> g -> Sudoku Vector DigSet
randomCompleted n g = head $ go g zeroIdx (emptySudoku n)
  where
    go :: g -> CellIdx -> Sudoku Vector DigSet -> [Sudoku Vector DigSet]
    go g idx s@(Sudoku n v)
      | not (validDS s')       = []
      | not (solvable s')      = []
      | solvedDS s'            = [s']
      | isPastLastIdx n idx    = [s']
      | isSingleton (s' ! idx) = go g (nextIdx n idx) s'
      | otherwise              = concatMap (go g2 (nextIdx n idx)) $ map (mkGuess idx s') $ filter (checkGuess idx s') (shuffle' digs (length digs) g1)
      where
        (g1,g2) = split g
        digs = digits (s' ! idx)
        s' = stuff s
        stuff su = let
          su1     = groupElimination su
          (i,su2) = hiddenSingles su1
                    in if su2 == su
                         then stuff2 su2
                         else stuff su2
        stuff2 su = let
          su1 = hiddenPairs su
          su2 = lockedCandidates1 su1
                     in if su2 == su
                          then su2
                          else stuff su2
    mkGuess idx (Sudoku n v) guess = Sudoku n (G.modify (writeGuessM (flatIndex n idx) (singletonDS guess)) v)
    writeGuessM idx guess v = GM.write v idx guess

removeRandom :: forall v g. (RandomGen g, G.Vector v DigSet) => Int -> g -> Sudoku v DigSet -> Sudoku v DigSet
removeRandom c g s@(Sudoku n v) = Sudoku n $ G.modify (\mv -> mapM_ (\i -> GM.write mv i allDigs) idxs) v
  where
    allDigs = allDigits n 
    idxs = take c $ shuffle' [0..n^4-1] (n^4) g


