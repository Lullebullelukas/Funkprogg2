module StringAlignment where

scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writers"
string2 = "vintner"

type AlignmentType = (String,String)

similarityScore :: String -> String -> Int
similarityScore [] [] = scoreMatch
similarityScore (x:xs) [] = similarityScore xs [] + scoreSpace 
similarityScore [] (y:ys) = similarityScore [] ys + scoreSpace
similarityScore (x:xs) (y:ys) = maximum [(similarityScore xs ys) + (score x y),
                                          similarityScore xs (y:ys) + scoreSpace,
                                          similarityScore (x:xs) ys + scoreSpace]

attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]


maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = [a | a <- xs, valueFcn a == maximum (map valueFcn xs)] 

score x y 
  | x == y = scoreMatch
  | x == '-' || y == '-' = scoreSpace
  | otherwise = scoreMismatch

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([],[])]
optAlignments (x:xs) [] = attachHeads x '-' (optAlignments xs [])
optAlignments [] (y:ys) = attachHeads '-' y (optAlignments [] ys) 
optAlignments (x:xs) (y:ys) = 
  maximaBy sc $ concat [attachHeads x y (optAlignments xs ys), 
                           attachHeads '-' y (optAlignments (x:xs) ys),
                           attachHeads x '-' (optAlignments xs (y:ys))]
            where
            sc :: AlignmentType -> Int
            sc ([],[]) = 0
            sc (x:xs,y:ys) = score x y + sc (xs,ys)   


outputOptAlignments string1 string2 = do 
 putStrLn $ "There are " ++ (show $ length (result)) ++ " optimal alignments:\n"
 putStrLn $ foldl1 (++) (map (\(x,y) -> x ++ "\n" ++ y ++ "\n\n") result) 
    where 
     result = optAlignments string1 string2
