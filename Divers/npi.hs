import Data.List
npi :: String -> Float
npi = head . foldl ff [] . words where   
            ff (x:y:ys) "*" = (x * y):ys
            ff (x:y:ys) "+" = (x + y):ys
            ff (x:y:ys) "-" = (y - x):ys
            ff (x:y:ys) "/" = (y / x):ys
            ff xs numberString = read numberString:xs
