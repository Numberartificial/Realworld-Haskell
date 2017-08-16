-- file "Ch04/InteractWith.hs"
import System.Environment (getArgs)
import Data.Char (toUpper, isUpper)
import Data.List --(isPrefixOf, isSuffixOf, null, length, head, tail)

--NOTE ghc --make InteractWith.hs produce the excutable file.
--NOTE this is function as the first class.
interactWith function inputFile outputFile = do
  input <- readFile inputFile
  let turn = function input
  writeFile outputFile turn

mainWith function = do
  args <- getArgs
  case args of
            [input, output] -> interactWith function input output
            _ -> putStrLn "error input"

main = mainWith myFunction
  where         myFunction = id

splitLines :: String -> [String]
splitLines cs =
  let (pre, suf) = break isLineTerminator cs
  in pre : case suf of --NOTE **
             ('\r': '\n' : rest) -> splitLines rest
             ('\r' : rest) -> splitLines rest
             ('\n' : rest) -> splitLines rest
             _ -> []

isLineTerminator c = c `elem` ['\r', '\n']

breakUpper cs = break isUpper cs

fixLines :: String -> String
fixLines input = unlines (splitLines input)

fixInteract = interactWith fixLines

--NOTE infix functions

a `plus` b = a + b

data a `Pair` b = a `Pair` b
                  deriving (Show)

-- we can use the constructor either prefix or infix,more like a operator
foo = Pair 1 2
bar = True `Pair` "quux"

infixOps = do
  let a = "abc" `isPrefixOf` "abcde"
      b = "de" `isSuffixOf` "abcde"
  print $ (a, b)

--NOTE List Functions: length,null,head, tail, last, init, (++), concat, reverse,and, or, all, any
listOps = do
  let xs = [1, 3..14]
      ys = splitAt 2 xs
      zs = takeWhile (< 3) xs
      hs = dropWhile (<= 3) xs
      a = 1 `elem` filter even xs
      b = zip "abcd" xs
      c = zipWith (+) ([1..]) xs
  print $ null xs
  print $ all (> 3) xs
  print $ all (>= 3) $ take 2 (tail xs)
  print $ any (< 2) $ drop 1 xs
  print $ span odd xs

--NOTE Partial and total functions, head is partial for not handling []
myDumbExample xs = if length xs > 0
                   then head xs  -- head [] will cause a Exception
                   else 'Z'

mySmartExample xs = if not (null xs)
                    then head xs
                    else 'Z'

myOtherExample (x:_) = x
myOtherExample [] = 'Z'

--NOTE STRING functions
str = unlines . lines
wrd = unwords . words

--NOTE Excersice
safeListFunc func [] = Nothing
safeListFunc func xs = Just (func xs)

safeHead = safeListFunc head
safeTail = safeListFunc tail
safeLast = safeListFunc last
safeInit = safeListFunc init

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith _ [] = []
splitWith p xs
  | null r = [ok]
  | null ok = splitWith p (tail r)
  | otherwise = ok : splitWith p (tail r)
  where (ok, r) = span p xs

firstWordOfEachLine :: String -> String
firstWordOfEachLine = unlines . map (head . words) . lines

--NOTE loop
loop acc [] = acc
loop acc (x:xs) = loop (acc * 10 + x) xs

upperCase2 xs = map toUpper xs

--NOTE left and right is associated with associative
{-Both foldr and foldl consume the list from left to right, so that's not the reason we refer to foldl as "left fold".

foldl _evaluates_ from left to right (left-associative)
foldr _evaluates_ from right to left (right-associative)-}
foldlD :: (a -> b -> a) -> a -> [b] -> a
foldlD step zero (x:xs) = foldlD step (step zero x) xs
foldlD _    zero []     = zero

foldrD step zero (x:xs) = step x (foldrD step zero xs)
foldrD _  zero [] = zero

mysum xs = foldlD (+) 0 xs
{-foldl (+) 0 (1:2:3:[])
          == foldl (+) (0 + 1)             (2:3:[])
          == foldl (+) ((0 + 1) + 2)       (3:[])
          == foldl (+) (((0 + 1) + 2) + 3) []
          ==           (((0 + 1) + 2) + 3)

foldr (+) 0 (1:2:3:[])
          == 1 +           foldr (+) 0 (2:3:[])
          == 1 + (2 +      foldr (+) 0 (3:[])
          == 1 + (2 + (3 + foldr (+) 0 []))
          == 1 + (2 + (3 + 0))
-}

--NOTE primitive recursive wicth can express as foldr
myFilter p xs = foldr step [] xs
    where step x ys | p x       = x : ys
                    | otherwise = ys

myMap p xs = foldr step [] xs
  where step x ys  = p x : ys

--NOTE these two expressions are worth thinking everyday.
foldlR f lzero xs = foldrD step id xs lzero
  where step x g a = g (f a x)

myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl fl z xs = (foldr fr id xs) z
  where l `fr` r = r . (`fl` l)

inf = [1..]
init1 = take 3 inf
