-- file: Haskell/Graphic/ex5.hs

import Data.List (sortBy)

length :: [a] -> Int
length xs = foldl (\x _ -> x + 1) 0 xs


doubleEach = map (*2)

pairAndOne xs = zip xs $ map (+1) xs 

addEachPair xs = [x+y | (x,y) <- xs]


maxList :: Ord a => [a] -> a
maxList = foldl1 max

minList :: Ord a => [a] -> a
minList = foldl1 min

addPointwise (a,b) (c,d) = (a+c, b+d)
addPairPointwise = foldl1 addPointwise


offset = fromEnum 'a'
maxcode = fromEnum 'z' - offset

encrypt xs = let orgmsg = map (flip (-) offset . fromEnum) xs
                 sndmsg = map (\x -> x+1 `mod` maxcode) orgmsg
             in sndmsg
                
decrypt :: [Int] -> String
decrypt xs = let rsvmsg = map (\x -> if x-1 < 0 then maxcode else x-1) xs
                 orgmsg = map (toEnum . (+) offset) rsvmsg
             in orgmsg
                
                
channel xs = xs == (decrypt . encrypt $ xs)                  

makeChange _ [] = []
makeChange amt (x:xs) = let (x',amt') = amt `divMod` x
                        in x' : makeChange amt' xs
