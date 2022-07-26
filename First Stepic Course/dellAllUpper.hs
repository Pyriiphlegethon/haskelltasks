delAllUpper :: String -> String
delAllUpper = unwords . map (filter (any (`elem` ['a'..'z']))) . words  