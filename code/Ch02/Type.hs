-- file Ch02/Type.hs
{- In response to Phil's question, and for general information for those not familiar with Unicode:

Unicode consists of "code points"; numeric values that represents a character or meta-character. This is purely an abstraction that dictates no specific digital representation.

How those numeric values are encoded and stored digitally is a separate issue. The main three encoding systems follow.

UTF-32 uses 32 bit "characters" to store each code point. A pro is that each "character" corresponds exactly to a code point. A con is that for English text, this quadruples the amount of storage space required, filling memory with a lot of zeroes.

UTF-16 uses 16-bit "characters" to encode code points. It requires either one or two "characters" to represent the entire Unicode set, with the most common values requiring a single "character". A pro is that it's more space-efficient than UTF-32. A con is that it's a variable-length encoding system.

UTF-8 uses 8-bit "characters" to encode code points. It's designed to be a superset of ASCII. It requires between one and four bytes to encode the entire Unicode set. A pro is that plain ASCII is UTF-8; no conversion is required. A con is that it's a variable-length encoding, and is inefficient for Asian languages compared to UTF-16 (requiring 3 bytes rather than two for most Asian text).
-}
char :: Char
char = 'a' -- NOTE UNICODE

{- NOTE Int is used for signed, fixed-width integer values. The exact range of values representable as Int depends on the system's longest “native” integer: on a 32-bit machine, an Int is usually 32 bits wide, while on a 64-bit machine, it is usually 64 bits wide.-}
int :: Int
int = 2

test = compare 1 2 == LT

list = head [ 1,2, 3, 4]

-- NOTE this will make a Exception
error1 = head []

string = tail "this is list of char"

tuple = ("Numberaritificial", 25)

emptyTuple = ()

-- NOTE, type error, testTuple = (True, "s") == ("s", True)
testEqul = (1, '1') == (1, '1')

listOps = do
  let a = take 3 [ 2, 21, 3]
      b = drop 1 a
      c = head b
      d = tail a
  print $ a
  print $ b
  print $ c
  print $ d

-- NOTE , fst / snd, only apply in pairs (which is a tuple with two parameters in parantheses)
tupleOps = do
  let a = ('0', 'a')
      ops = [ fst , snd]
  print $ map (\f-> f a) ops

testOps = do
  let a = lines "the \n line \n is split"
  print $ a

-- NOTE In haskell , there is no side effects: the result of function depends only on the inputs that we explicity provide. Pure function.


myDrop n xs = if n <= 0 || null xs
              then xs
              else myDrop (n - 1) (tail xs)

isOdd :: Integral a => a -> Bool
isOdd n = mod n 2 == 1


-- NOTE None-strict evaluation, often referred to lazy evaluation.
newOr a b = if a then a else b
testLazy = newOr True (length [1..] > 0)

--NOTE polymorphic, parameterised polymorphic. coercion polymorphic, subtype polymorphic(java/C++) not in haskell.
polyOps = do
  let a = [1, 2, 3, 4]
      b = ['a', 'b', 'c']
      c = truncate $ fromIntegral 1
  print $ last a
  print $ last b

--NOTE -> is right assocaitive, newAdd :: Int -> (Int -> Int). Partial function application and currying.
newAdd :: Int -> Int -> Int
newAdd = (+)

add3 :: Int -> Int
add3 = newAdd 3

--NOTE exercise2, will fail when length xs <= 1.
lastButOne [x:y:[]] = x
lastButOne xs@[x:y:ys] = lastButOne $ tail xs --[y:ys]
lastButOne _ = error "length less than 2"

-- NOTE Why the fuss over purity? I hate impurities.
{- Beacause of Purity, we can get a strong hint of what a pure function does by simply reading its name and understanding its type signature which is a nuclear bomb during your daily "battle" to save you from verbose apis and documents-}

--“If it walks like a duck, and quacks like a duck, then let's call it a duck.”  so funny. Whirlwind overview, in pure function world, you almost get a real duck; a fuck in impure world.

