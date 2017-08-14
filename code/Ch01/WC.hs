-- file Ch01/WC.hs
-- NOTE lines beginning with "--" are comments
-- NOTE In GHCI, :set (prompt/ +t)
-- NOTE :show bindings, show 'it' bindings

main :: IO ()
main = interact wordCount
  where wordCount input = show (length (lines input)) ++ "\n"

exercise1 :: IO ()
exercise1 = do
  let a = succ 7 + pred 7 + round  (sqrt 15)
      b = sqrt 16
      c = truncate pi
      d = round pi + ceiling pi + floor 4.3
  print $ a
  print $ b
  print $ c
  print $ d

filePath = "Ch01/text.txt"

exercise2 :: IO ()
exercise2 = do
  content <- readFile filePath
  let num = length $ words content
  print $ num

exercise3 :: IO ()
exercise3 = do
  content <- readFile filePath
  let num = length content
  print $ num
