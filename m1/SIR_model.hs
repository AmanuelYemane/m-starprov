module M1 where 

-- 1 SIR-model

-- Funktion som räknar ut simulerar en bolkning under en pandemi (Susceptible, Infected och Recovered)
sirSimulate :: Double -> Double -> Int -> Int -> (Int, Int, Int) -> [(Int, Int, Int)]
sirSimulate _ _ _ 0 _ = [] -- Klar när vi har nått totala antalet dagar (steps=antalet dagar)
sirSimulate beta gamma n steps initialState = 
    -- Lista med vår första state, röknar rekrisivt slla andra stätes för alla dagar 
    initialState : sirSimulate beta gamma n (steps-1) (mottagliga beta n initialState, smittade beta gamma n initialState, återhämtade gamma initialState)


mottagliga :: Double -> Int -> (Int, Int, Int) -> Int
-- Formel för att räkna ut nytt värde för mottagliga 
mottagliga beta n (s, i, _) = s - round((beta * fromIntegral s * fromIntegral i)/ fromIntegral n)


smittade :: Double -> Double -> Int -> (Int, Int, Int) -> Int
-- Formel för att räkna ut nytt värde för smittade
smittade beta gamma n (s, i, _) =  i + round((beta * fromIntegral s * fromIntegral i)/ fromIntegral n - (gamma * fromIntegral i))

återhämtade :: Double -> (Int, Int, Int) -> Int
-- Formel för att räkna ut nytt värde för smittade återhämtade
återhämtade gamma (_, i, r) = r + round(gamma * fromIntegral i)


-- 2 Remove every n:th element
removeEveryNth :: Int -> [a] -> [a]
removeEveryNth _ [] = [] -- Basecase: If list is empty return an empty list
removeEveryNth n v    
    | n > length v = v -- If n is greater than the lenght of the list v ruturn empty listthe the 
    | n == 1 = [] -- If n is 1, return an empty list
    | otherwise = removeNthHelper n v 1 -- Remove every n:th element, introduce index which starts as 1

removeNthHelper :: Int -> [a] -> Int -> [a]
removeNthHelper _ [] _ = []
removeNthHelper n (head:tail) index
    | index `mod` n == 0 = removeNthHelper n tail (index+1) -- If the index is evenly divisible by n, skip that element by calling removeNthHelper on the tail
    | otherwise = head : removeNthHelper n tail (index+1) -- Otherwise add head to the list and call and removeNthHelper on the tail


-- 3 No lower case 
-- Introduce list of small letters defined globally
smallChars :: String
smallChars = ['a'..'z'] 

-- Returns a list if strings not containing small chars
noLowercaseStrings :: [String] -> [String]
noLowercaseStrings [] = []
noLowercaseStrings (head:tail) -- Spits the list into the first sting and the rest of the strings
    | lowerCaseChecker head = noLowercaseStrings tail -- If there is a lower case char, skip that string and move on
    | otherwise = head : noLowercaseStrings tail -- No small char, add that string to the list and check other strings

lowerCaseChecker :: String -> Bool -- Bool function that returns true if a char in a string is small
lowerCaseChecker [] = False -- Basecase
lowerCaseChecker (head:tail) -- Splits each string into chars in order to chech for small chars (Input is a string from the list)
    | head `elem` smallChars = True -- If head a char from string is also part of the list return True
    | otherwise = lowerCaseChecker tail -- if the head if the string is not lower continue checking the rest of the String


longestString :: [String] -> String
longestString [] = []
longestString v = foldl stringComparison "" v

stringComparison :: String -> String -> String
stringComparison string v
    | length v > length string = v
    | otherwise = string


-- Define the Vec3 type
data Vec3 = Vec3 Double Double Double
  deriving (Show, Eq)

-- Implement vector operations using an instance of Num
instance Num Vec3 where
  (Vec3 x1 y1 z1) + (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
  (Vec3 x1 y1 z1) - (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)
  (Vec3 x1 y1 z1) * (Vec3 x2 y2 z2) = Vec3 (x1 * x2) (y1 * y2) (z1 * z2) -- Not standard vector multiplication
  negate (Vec3 x y z) = Vec3 (-x) (-y) (-z)
  abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
  signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)
  fromInteger n = Vec3 (fromInteger n) (fromInteger n) (fromInteger n)

reflect :: Vec3 -> Vec3 -> Vec3
reflect incoming normal = incoming - scalarMul (2.0 * dotProduct incoming normal) normal

dotProduct :: Vec3 -> Vec3 -> Double
dotProduct (Vec3 x1 x2 x3) (Vec3 y1 y2 y3) = x1 * y1 + x2 * y2 + x3 * y3

scalarMul :: Double -> Vec3 -> Vec3
scalarMul double (Vec3 x y z) = Vec3 (double * x) (double * y) (double * z) 


-- removeNegativeZ :: [Vec3] -> [Vec3]
-- removeNegativeZ [] = []
-- removeNegativeZ (head:tail) 
--     | isNegative head = removeNegativeZ tail
--     | otherwise = head : removeNegativeZ tail

-- isNegative :: Vec3 -> Bool
-- isNegative (Vec3 x y z) 
--     | z < 0 = True
--     | otherwise = False  

removeNegativeZ :: [Vec3] -> [Vec3]
removeNegativeZ = isNegative (\(Vec3 _ _ z) -> z >= 0)

isNegative :: (Vec3 -> Bool) -> [Vec3] -> [Vec3]
isNegative _ [] = []
isNegative function (head:tail) 
    | function head = head : isNegative function tail
    | otherwise = isNegative function tail 



