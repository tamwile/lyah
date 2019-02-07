
-- syntaxe des fonctions

lucky :: (Integral a) => a -> String
lucky 7 = "lucky number seven"
lucky x = "sorry, out of luck"


factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)


addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors a b = (fst a + fst b, snd a + snd b)

addVectors' :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors' (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)


first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z


head' :: [a] -> a
head' [] = error "Can't call head on an empty list"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "The list is empty"
tell (x:[]) = "The list has one element : " ++ show x
tell (x:y:[]) = "The list has two elements : " ++ show x ++ " and " ++ show y
tell (x:y:_) = "This list is too long. The first two elements are : " ++ show x ++ " and " ++ show y

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs


--  Motifs nommés

capital :: String -> String
capital "" = "Empty string, whoops"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]


-- gardes

-- bmiTell :: (RealFloat a) => a -> a -> String 
-- bmiTell weight height
--   | weight / height ^ 2 <= 18.5 = "You're underweight"
--   | weight / height ^ 2 <= 25.0 = "You're normal"
--   | weight / height ^ 2 <= 30.0 = "You're fat"
--   | otherwise   = "You're a whale" -- that's mean


max' :: (Ord a) => a -> a -> a
max' a b
  | a > b     = a
  | otherwise = b


myCompare :: (Ord a ) => a -> a -> Ordering
a `myCompare` b
  | a > b     = GT
  | a == b    = EQ
  | otherwise = LT


-- where keyword


bmiTell :: (RealFloat a) => a -> a -> String 
bmiTell weight height
  | bmi <= skinny = "You're underweight"
  | bmi <= normal = "You're normal"
  | bmi <= fat = "You're fat"
  | otherwise   = "You're a whale" -- that's mean
  where bmi = weight / height ^ 2
        (skinny, normal, fat) = (18.5, 25.0, 30.0)


initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f:_) = firstname
        (l:_) = lastname

list_bmis :: [(Float, Float)]
list_bmis = [(85, 1.90), (62, 1.84), (52, 1.75), (40, 1.59)]

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [ bmi w h | (w, h) <- xs]
  where bmi weight height = weight / height ^ 2


calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [ bmi w h | (w, h) <- xs, let bmi w h = w / h ^ 2]


cylinder :: (RealFloat a ) => a -> a -> a
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r^2
  in sideArea + 2 * topArea


-- Expression case

head1 :: [a] -> a
head1 [] = error "No head for empty list"
head1 (x:_) = x

-- Les 2 fonctions sont interchangeables
-- Equivalent à : 

head2 :: [a] -> a
head2 xs = case xs of [] ->  error "No head for empty list"
                      (x:_) -> x


describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of []  -> "empty"
                                               [x] -> "a singleton list"
                                               xs  -> "a longer list"


