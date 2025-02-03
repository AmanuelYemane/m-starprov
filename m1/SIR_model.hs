module M1 where 

-- 1 SIR-model


sirSimulate :: Double -> Double -> Int -> Int -> (Int, Int, Int) -> [(Int, Int, Int)]
sirSimulate _ _ _ 0 _ = []
sirSimulate beta gamma n steps initialState = 
    initialState : sirSimulate beta gamma n (steps-1) (mottagliga beta n initialState, smittade beta gamma n initialState, återhämtade gamma initialState)


mottagliga :: Double -> Int -> (Int, Int, Int) -> Int
mottagliga beta n (s, i, _) = s - round((beta * fromIntegral s * fromIntegral i)/ fromIntegral n)

smittade :: Double -> Double -> Int -> (Int, Int, Int) -> Int
smittade beta gamma n (s, i, _) =  i + round((beta * fromIntegral s * fromIntegral i)/ fromIntegral n - (gamma * fromIntegral i))

återhämtade :: Double -> (Int, Int, Int) -> Int
återhämtade gamma (_, i, r) = r + round(gamma * fromIntegral i)


-- 2 Remove every n:th element

removeEveryNth :: Int -> [a] -> [a]
removeEveryNth _ [] = []
removeEveryNth n v    
    | n > length v = [] 
    | otherwise = removeNthHelper n v 1 

removeNthHelper :: Int -> [a] -> Int -> [a]
removeNthHelper _ [] _ = []
removeNthHelper n (head:tail) count
    | count `mod` n == 0 = removeNthHelper n tail (count+1)
    | otherwise = head : removeNthHelper n tail (count+1) 

-- 3 No lower case 


smallChars :: String
smallChars = ['a'..'z']

noLowercaseStrings :: [String] -> [String]
noLowercaseStrings [] = []
noLowercaseStrings (head:tail) 
    | lowerCaseChecker head = noLowercaseStrings tail 
    | otherwise = head : noLowercaseStrings tail

lowerCaseChecker :: String -> Bool
lowerCaseChecker [] = False
lowerCaseChecker (head:tail)
    | head `elem` smallChars = True
    | otherwise = lowerCaseChecker tail 