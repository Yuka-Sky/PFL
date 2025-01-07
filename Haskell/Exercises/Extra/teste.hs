transforma :: String -> String
transforma [] = []
transforma (x:xs)
   | x=='a' = x:'p':x : transforma xs
   | x=='e' = x:'p':x : transforma xs
   | x=='i' = x:'p':x : transforma xs
   | x=='o' = x:'p':x : transforma xs
   | x=='u' = x:'p':x : transforma xs
   | otherwise = x:transforma xs
