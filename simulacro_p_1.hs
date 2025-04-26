borrarIdx :: Int -> [a] -> [a]
borrarIdx _ [] = []
borrarIdx 0 (x : xs) = xs
borrarIdx i (x : xs) = x : borrarIdx (i - 1) xs
