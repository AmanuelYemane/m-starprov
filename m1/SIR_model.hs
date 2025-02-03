module SIR where 


-- 1.1


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