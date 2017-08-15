import Data.List (sortBy)
import Data.Ord (comparing)

--NOTE BookInfo is a Type Constructor which must start with a capital letter
--NOTE Book is a Value Constructor whci must also start with a capital letter
--NOTE both Type Constructor and Value Constuctor are function.Hense, there is no more other thing except function in Pure Function world.
--NOTE By positional, we mean that the section number is in the first field of the Haskell type, and the title is in the second. We refer to them by location, not by name.
data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

book1 = Book 1 "haskell" ["curry"]

testBook = do
  show $ Book 1 "ok" []

--NOTE it is normal to make Value Constructor the same name with Type Consturcotr.
data BookReview = BookReview BookInfo CustomerID ReviewBody

--NOTE Type synonym, a descriptive name of exsisting type
type CustomerID = Int
type ReviewBody = String

type BookRecord = (BookInfo, BookReview)

--NOTE algebraic data type
data Mood = Happy | Sad

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

--NOTE descriminated union
type Vector = (Double, Double)

data Shape = Circle Vector Double
           | Poly [Vector]

--NOTE Deconsturcte type. Pattern matching.
{-1. matching Value Constructor
  2. matching data components values-}
myNot True = False
myNot False = True

--NOTE [1,2,3] == (1 : (2 : (3 : [])))
--NOTE Ordering/Location is important
sumList (x:xs) = 1 + sumList xs
sumList [] = 0

third (_, _, c) = c

--NOTE consturction and deconstruction point to structure not memory.
inside :: Num t3 => (t2, t1, [t]) -> t3
inside (a, b, (x : xs)) = sumList xs

--NOTE GHC compile option, -fwarn-incomplete-patterns
{- badPattern [] will cause Exption: Non-exhaustive patterns in function pattern-}
badPattern (x:xs) = x

--NOTE Record, no more boilerplate,bulky,irksome,tedious,repetitive. And then Lens.
data Customer = Customer {
      customerID      :: CustomerID
    , customerName    :: String
    , customerAddress :: Address
    } deriving (Show)

customer1 = Customer 271828 "J.R. Hacker"
            ["255 Syntax Ct",
             "Milpitas, CA 95134",
             "USA"]

--NOTE we can vary order in which list fields.
customer2 = Customer {
              customerAddress = ["1048576 Disk Drive",
                                 "Milpitas, CA 95134",
                                 "USA"]
            , customerID = 271828
            , customerName = "Jane Q. Citizen"
            }

-- data Maybe a = Just a | Nothing

--NOTE null or nil is a bad thing.
data Olist a = Con a (Olist a) | Empty
               deriving (Show)

olist = Con 1 Empty

data Tree a = Node a (Tree a) (Tree a)
            | Leaf
              deriving (Show)

fromList :: [a] -> Olist a
fromList (x:xs) = Con x $ fromList xs
fromList [] = Empty

toList :: Olist a -> [a]
toList (Con a xs) = a : (toList xs)
toList Empty = []

data MaybeTree a = MaybeTree (Maybe a)

terminator = error "no callback, running done"

safeSecond :: [a] -> Maybe a

safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
                then Nothing
                else Just (head (tail xs))

tidySecond :: [a] -> Maybe a

tidySecond (_:x:_) = Just x
tidySecond _       = Nothing

--NOTE Closure, let bindings a expression(a couple of ).
lend amount balance = let reserve    = 100
                          newBalance = balance - amount
                      in if balance < reserve
                         then Nothing
                         else Just newBalance


--NOTE this is a 't' because there is no use of function parameter 'a' in this function. USE GHC option -fwarn-name-shadowing to see the shadowing name.
quux :: t -> [Char]
quux a = let a = "foo"
         in a ++ "eek!"

nodesAreSame (Node a _ _) (Node b _ _)
  | a == b = Just a
  | otherwise = Nothing

nodesAreSame _ _ = Nothing

lend3 amount balance
     | amount <= 0            = Nothing
     | amount > reserve * 0.5 = Nothing
     | otherwise              = Just newBalance
    where reserve    = 100
          newBalance = balance - amount

niceDrop n xs | n <= 0 = xs
niceDrop _ []          = []
niceDrop n (_:xs)      = niceDrop (n - 1) xs

--NOTE EXERCISE

length' :: Num t => [t1] -> t
length' [] = 0
length' (_:xs) = 1 + length' xs

palindrome xs = xs ++ reverse xs

isPalindrome xs = xs == reverse xs

sortByLenth xs =
  sortBy (comparing length) xs

intersperse seperator (x:xs) = foldl (\b a -> b ++ [seperator] ++ a) x xs
intersperse seperator _ = []

height (Node a left right) =
  maximum [height left, height right] + 1
height Leaf = 1

data Point t = Point t t

data Direction = DLeft | DRight | DStraight

turn ::
  (Num t, Ord t) => Point t -> Point t -> Point t -> Direction
turn (Point ax ay) (Point bx by) (Point cx cy)
  | dp > 0 = DLeft
  | dp < 0 = DRight
  | dp == 0 = DStraight
  where dp = ((ay - by)*(cx - bx)) + ((bx - ax)*(cy - by))

turns l@(a:b:c:xs) = (turn a b c) : (turns $ tail l)
turns _ = []
