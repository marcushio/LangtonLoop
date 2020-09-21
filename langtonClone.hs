import Data.Char
import Data.Array
import System.IO
import System.Console.ANSI



{-Langton's loop. Recreate Langton's loop. It must be said early and emphatically, 
I leaned very heavily on the example of Life used by Lance Williams to get this going-}

-- Code used for arrays ________________________________________________

--structural tools
rowIndices a = [rMin..rMax] where ((rMin, _), (rMax, _)) = bounds a
colIndices a = [cMin..cMax] where ((_, cMin), (_, cMax)) = bounds a
row a j = [a ! (j,k) | k <- colIndices a]

emptyArray init n m = array b [(i,init) | i <- range b]
    where b = ((0,0),(n-1,m-1))

---------------------
--Conversion tools 
lulzToArray :: [[Char]] -> Array (Int,Int) Char
lulzToArray xs@(x:_) = array b [(i, (xs !! j) !! k) | i@(j,k) <- range b]
    where b = ((0,0),(n,n))
          n = length xs - 1

arrayToLulz :: Array (Int,Int) Char -> [[Char]]
arrayToLulz a = [row a j | j <- rowIndices a]

digitToIntArray = fmap digitToInt
intToDigitArray = fmap intToDigit
-- I have to figure out color for mine 

intToLifeArray a = fmap (\x -> if x == 0 then ' ' else intToDigit x) a 


---------------
--printing tools 
printArray :: Array (Int,Int) Char -> IO ()
printArray a = sequence_ $ fmap putStrLn (arrayToLulz a)

putStrFatLn cs = do
  sequence_ [putChar c >> putChar ' ' | c <- cs]
  putChar '\n'

printArrayFat a = sequence_ $ fmap putStrFatLn (arrayToLulz a)

printIntArray :: Array (Int,Int) Int -> IO ()
printIntArray = printArray . intToDigitArray

-----------------------
-- *********** repurpose this main structural stuff 
(%) :: Array (Int,Int) a -> (Int,Int) -> a
a % (j,k) = a ! (j `mod` (m+1),k `mod` (n+1)) where (m,n) = snd $ bounds a
--remember the langton table is in a little different order
--[center, never, eat, soggy, waffles]
neighbors a (j,k) =
    [a ! (j + 0, k + 0) , a % (j - 1, k + 0), a % (j + 0, k + 1),a % (j + 1, k + 0),a % (j + 0, k - 1)]
 {-    [                    a % (j - 1, k + 0),   
     a % (j + 0, k - 1), a ! (j + 0, k + 0) , a % (j + 0, k + 1),
                         a % (j + 1, k + 0)                     ]  -}
   

--neighborsArray :: Num e => Array (Int, Int) e -> Array (Int, Int) e
neighborsArray a = array (bounds a) [ (i, neighbors a (j,k)) | i@(j,k) <- indices a] 

langtonRules :: [Int] -> [([Char], Char)] -> Int
langtonRules hood (x:[]) = 0 --this should never happen... but I'm getting a non exhaustive pattern
langtonRules hood rules@(x:xs) = if (map digitToInt (fst x)) == hood then (digitToInt $ snd x)
                                    else langtonRules hood xs 

--zipWithArray :: (a -> b -> c) -> Array (Int,Int) a -> Array (Int, Int) b -> Array (Int,Int) c
--zipWithArray f a b = array (bounds a) [(i, f (a ! (j,k)) b ) | i@(j,k) <- indices a]

updateWorld world daRulez = nextWorld langtonRules (neighborsArray world) daRulez where 
    nextWorld f a rules = array (bounds a) [(i, f (a ! (j,k)) rules ) | i@(j,k) <- indices a]
 
--lifeLoop :: Array (Int, Int) Int -> IO b
langtonLoop :: [([Char], Char)] -> Array (Int, Int) Int -> IO b
langtonLoop daRulez a  = do
  setCursorPosition 0 0
  printArrayFat $ intToLifeArray a
  --print daRulez
  --printArray $ intToLifeArray a
  langtonLoop daRulez (updateWorld a daRulez) 

insert a a' = a' // assocs a
main = do
  givenTable <- readFile "langton-table.txt"
  --transition table is currently a lulz
  let daRulez = makeLookup $ (splitText (/='\n') givenTable) where 
      splitText _ [] = []
      splitText pred xs@(x:xs') = let e = takeWhile pred xs in e: (splitText pred (drop (length e+1) xs) )
  --print daRulez --so i can see if it did it right
  clearScreen
  hideCursor
  langtonLoop daRulez (insert (digitToIntArray $ lulzToArray startLoop) (emptyArray 0 83 135) )

--this was running... just not transitioning right. 
{- insert a a' = a' // assocs a
main = do
  givenTable <- readFile "langton-table.txt"
  --transition table is currently a lulz
  let daRulez = makeLookup $ (splitText (/='\n') givenTable) where 
      splitText _ [] = []
      splitText pred xs@(x:xs') = let e = takeWhile pred xs in e: (splitText pred (drop (length e+1) xs) )
  --print daRulez --so i can see if it did it right
  clearScreen
  hideCursor
  langtonLoop daRulez (insert (digitToIntArray $ lulzToArray startLoop) (emptyArray 0 83 135) ) -}

xState x = [x!!0, x!!1, x!!2, x!!3, x!!4]
perm0 x  = [x!!0, x!!4, x!!1, x!!2, x!!3]
perm1 x  = [x!!0, x!!3, x!!4, x!!1, x!!2]
perm2 x  = [x!!0, x!!2, x!!3, x!!4, x!!1]
next x = x !! 5 
makeLookup (x:[]) = [(xState x, (next x)),(perm0 x, (next x)), (perm1 x, (next x)), (perm2 x,  (next x))]
makeLookup (x:xs) = [(xState x, (next x)),(perm0 x, (next x)), (perm1 x, (next x)), (perm2 x,  (next x))] ++ (makeLookup xs)



startLoop = ["022222222000000",
             "217014014200000",
             "202222220200000",
             "272000021200000",
             "212000021200000",
             "202000021200000",
             "272000021200000",
             "212222221222220",
             "207107107111112",
             "022222222222220"]