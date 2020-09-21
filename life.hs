import Data.Char
import Data.Array
import System.IO
import System.Console.ANSI


lulzToArray :: [[Char]] -> Array (Int,Int) Char
lulzToArray xs@(x:_) = array b [(i, (xs !! j) !! k) | i@(j,k) <- range b]
    where b = ((0,0),(n,n))
          n = length xs - 1

blinker = ["000000000",
           "000000000",
           "000000000",
           "000010000",
           "000010000",
           "000010000",
           "000000000",
           "000000000",
           "000000000"]

rowIndices a = [rMin..rMax] where ((rMin, _), (rMax, _)) = bounds a
colIndices a = [cMin..cMax] where ((_, cMin), (_, cMax)) = bounds a

row a j = [a ! (j,k) | k <- colIndices a]

arrayToLulz :: Array (Int,Int) Char -> [[Char]]
arrayToLulz a = [row a j | j <- rowIndices a]

printArray :: Array (Int,Int) Char -> IO ()
printArray a = sequence_ $ fmap putStrLn (arrayToLulz a)

putStrFatLn cs = do
  sequence_ [putChar c >> putChar ' ' | c <- cs]
  putChar '\n'

printArrayFat a = sequence_ $ fmap putStrFatLn (arrayToLulz a)

emptyArray init n m = array b [(i,init) | i <- range b]
    where b = ((0,0),(n-1,m-1))

inset a a' = a' // assocs a

digitToIntArray = fmap digitToInt
intToDigitArray = fmap intToDigit

printIntArray :: Array (Int,Int) Int -> IO ()
printIntArray = printArray . intToDigitArray

(%) :: Array (Int,Int) a -> (Int,Int) -> a
a % (j,k) = a ! (j `mod` (m+1),k `mod` (n+1)) where (m,n) = snd $ bounds a

neighbors a (j,k) =
  sum [a % (j - 1, k - 1), a % (j - 1, k + 0), a % (j - 1, k + 1),
       a % (j + 0, k - 1),           0       , a % (j + 0, k + 1),
       a % (j + 1, k - 1), a % (j + 1, k + 0), a % (j + 1, k + 1)]

neighborsArray a = array (bounds a) [ (i, neighbors a (j,k)) | i@(j,k) <- indices a]

lifeRules 0 3 = 1
lifeRules 0 _ = 0
lifeRules 1 0 = 0
lifeRules 1 1 = 0
lifeRules 1 2 = 1
lifeRules 1 3 = 1
lifeRules 1 _ = 0

zipWithArray :: (a -> b -> c) -> Array (Int,Int) a -> Array (Int,Int) b -> Array (Int,Int) c
zipWithArray f a b = array (bounds a) [(i, f (a ! (j,k)) (b ! (j,k)) ) | i@(j,k) <- indices a]

lifeUpdate a = zipWithArray lifeRules a (neighborsArray a)

life = '\x25EF'

--life = '\x29BF'

intToLifeArray = fmap $ \x -> if x == 0 then ' ' else life

lifeLoop a = do
  setCursorPosition 0 0
  printArrayFat $ intToLifeArray a
  lifeLoop $ lifeUpdate a

main = do
  clearScreen
  hideCursor
  lifeLoop $ inset (digitToIntArray $ lulzToArray rPenta') (emptyArray 0 83 135)

glider' = ["000000000",
           "000000000",
           "000000000",
           "000010000",
           "000001000",
           "000111000",
           "000000000",
           "000000000",
           "000000000"]

rPenta' = ["000000000",
           "000000000",
           "000000000",
           "000011000",
           "000110000",
           "000010000",
           "000000000",
           "000000000",
           "000000000"]
