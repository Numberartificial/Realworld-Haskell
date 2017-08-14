
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
