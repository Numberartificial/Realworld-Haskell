-- file "Ch04/InteractWith.hs"
import System.Environment (getArgs)
import Data.Char (isUpper)
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
