module StringAlignment where

--Values for scoring for testing
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



attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails t1 t2 aList = [(xs ++ [t1],ys ++ [t2]) | (xs,ys) <- aList]
 
test = outputOptAlignments string1 string2

similarityScore' xs ys = simScore (length xs) (length ys)
  where
    simScore i j = simTable!!i!!j
    simTable = [[ simEntry i j | j<-[0..]] | i<-[0..] ]

    simEntry i 0 = i*scoreSpace
    simEntry 0 j = j*scoreSpace
    simEntry i j =
      maximum [(simScore (i-1) (j-1)) + (score x y),
      simScore (i-1) j + scoreSpace,
      simScore i (j-1) + scoreSpace]

      where
         x = xs!!(i-1)
         y = ys!!(j-1)


optAlignments' :: String -> String -> [AlignmentType]
optAlignments' xs ys = snd $ optAlign (length xs) (length ys)
  where
    optAlign i j = optTable!!i!!j
    optTable = [[ optEntry i j | j <- [0..]] | i <-[0..] ]


    optEntry :: Int -> Int -> (Int, [AlignmentType])
    optEntry 0 0 = (0, [([],[])])
    optEntry i 0 = (scoreSpace + (fst (optEntry (i-1) 0)), attachTails (xs!!(i-1)) '-' (snd (optEntry (i-1) 0)))
    optEntry 0 j = (scoreSpace + (fst (optEntry 0 (j-1))), attachTails '-' (ys!!(j-1)) (snd (optEntry 0 (j-1))))
    optEntry i j = (points, best) 
                
       where
         temp = [((fst (optAlign (i-1) (j-1))) + (score x y), attachTails x y $ snd (optAlign (i-1) (j-1))),
                  ((fst (optAlign (i-1) j)) + scoreSpace, attachTails '-' y $ snd (optAlign (i-1) j)),
                  ((fst (optAlign i (j-1))) + scoreSpace, attachTails x '-' $ snd (optAlign i (j-1)))]
 
         points = maximum $ map fst temp 
         best = concat $ map snd [a | a <- temp, ((fst a) == points)] 

         x = xs!!(i-1)
         y = ys!!(j-1)
