import Data.Char
import Data.Array
import System.IO
import System.Random
import System.Console.ANSI

{-Langton's loop. Recreate Langton's loop. It must be said early and emphatically, 
I leaned very heavily on the example of Life used by Lance Williams to get this going-}

--________  Array Printing Stuff  ________________________
printArray :: Array (Int,Int) Char -> IO ()
printArray a = sequence_ $ fmap putStrLn (arrayToLulz a)

putStrFatLn cs = do
  sequence_ [putChar c >> putChar ' ' | c <- cs]
  putChar '\n'

printArrayFat a = sequence_ $ fmap putStrFatLn (arrayToLulz a)

--life = '\x29BF'
intToLifeArray = fmap $ \x -> if x == 0 then ' ' else '\x25EF'
--________  Array Conversions ______________________________
rowIndices a = [rMin..rMax] where ((rMin, _), (rMax, _)) = bounds a
colIndices a = [cMin..cMax] where ((_, cMin), (_, cMax)) = bounds a

row a j = [a ! (j,k) | k <- colIndices a]

lulzToArray :: [[Char]] -> Array (Int,Int) Char
lulzToArray xs@(x:_) = array b [(i, (xs !! j) !! k) | i@(j,k) <- range b]
    where b = ((0,0),(n,n))
          n = length xs - 1

arrayToLulz :: Array (Int,Int) Char -> [[Char]]
arrayToLulz a = [row a j | j <- rowIndices a]

digitToIntArray = fmap digitToInt
intToDigitArray = fmap intToDigit

--given
--updateCell :: arr -> a 
--updateCell array =  
--I assume input to be an array 
--langtonLoop a = 

zipWithArray :: (a -> b -> c) -> Array (Int,Int) a -> Array (Int,Int) b -> Array (Int,Int) c
zipWithArray f a b = array (bounds a) [(i, f (a ! (j,k)) (b ! (j,k)) ) | i@(j,k) <- indices a]

langtonUpdate a = zipWithArray lifeRules a (neighborsArray a)

langtonLoop a = do
  setCursorPosition 0 0
  printArrayFat $ intToLifeArray a
  langtonLoop $ langtonUpdate a


--put our starting state into the world 
insert a a' = a' // assocs a
--use symmetry to complete our transition table

  --a neighborhood and all it's symettries (x1,x2,x3)
finishTable (x : [])  = [x, x1, x2, x3]
  where x1 = [x!!0, x!!4, x!!1, x!!2, x!!3]
        x2 = [x!!0, x!!3, x!!4, x!!1, x!!2]
        x3 = [x!!0, x!!2, x!!3, x!!4, x!!1]
finishTable (x : xs) = [x,x1,x2,x3] ++ (finishTable xs)
  --just shift our string around 
  where x1 = [x!!0, x!!4, x!!1, x!!2, x!!3]
        x2 = [x!!0, x!!3, x!!4, x!!1, x!!2]
        x3 = [x!!0, x!!2, x!!3, x!!4, x!!1]

main = do
  givenTable <- readFile "langton-table.txt"
  let transitionTable = (splitText (/='\n') givenTable) where 
            splitText _ [] = []
            splitText pred xs@(x:xs') = let e = takeWhile pred xs in e: (splitText pred (drop (length e +1) xs) ) 
  clearScreen
  hideCursor
  --langtonLoop $ insert (digitToIntArray $ lulzToArray startLoop) (emptyArray 0 83 135) --start arr
  --  where emptyArray init n m = array ((0,0),(n-1,m-1)) [(i,init) | i <- range ((0,0),(n-1,m-1))]



