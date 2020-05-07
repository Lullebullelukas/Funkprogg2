module StringAlignment where
--assignment 2 by Simon and Lukas
--The hardest part of the assignment was the optimization of with tables on optAlignments'.
--It was challenging to understand how the table is contstructed and how to use it. It was also hard to try to test things whilst the function was in its infancy
--The part code that we were most proud of was the function maximaBy since we think list comprehension looks neat.


--Values for scoring: for testing
scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1
string1 = "writers"
string2 = "vintner"



type AlignmentType = (String,String)


--Non-optimized. Assigns a score that describes the similarity between two strings depending on the constants above.  
similarityScore :: String -> String -> Int
similarityScore [] [] = scoreMatch
similarityScore (x:xs) [] = similarityScore xs [] + scoreSpace 
similarityScore [] (y:ys) = similarityScore [] ys + scoreSpace
similarityScore (x:xs) (y:ys) = maximum [similarityScore xs ys + score x y,
                                         similarityScore xs (y:ys) + scoreSpace,
                                         similarityScore (x:xs) ys + scoreSpace]


--Attaches the heads h1 and h2 to the tuples in the list aList where h1 attaches to the fst and h2 to snd
attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]


--Applies a value function to a list and returns a list of the entries which have the max value when the function is applied
maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = [a | a <- xs, valueFcn a == maximum  (map valueFcn xs)] 


--Scores the compared Chars according to the constants above 
score x y 
  | x == y = scoreMatch
  | x == '-' || y == '-' = scoreSpace
  | otherwise = scoreMismatch


--Non-optimized. Returns a list of the alignments of the input strings with the highest score 
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

--Outputs the alignments formatted for the human eye
outputOptAlignments string1 string2 = do 
 putStrLn $ "There are " ++ show (length result) ++ " optimal alignments:\n"
 putStrLn $ foldl1 (++) (map (\(x,y) -> x ++ "\n" ++ y ++ "\n\n") result) 
    where 
     result = optAlignments string1 string2


--Helper function. Attaches the tails t1 and t2 to the tuples in a list. 
attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails t1 t2 aList = [(xs ++ [t1],ys ++ [t2]) | (xs,ys) <- aList]


--Optimized version of the above function. Uses a table to store already calculated values 
similarityScore' xs ys = simScore (length xs) (length ys)
  where
    simScore i j = simTable!!i!!j
    simTable = [[ simEntry i j | j<-[0..]] | i<-[0..] ]

    simEntry i 0 = i*scoreSpace
    simEntry 0 j = j*scoreSpace
    simEntry i j =
      maximum [simScore (i-1) (j-1) + score x y,
               simScore (i-1) j + scoreSpace,
               simScore i (j-1) + scoreSpace]

      where
         x = xs!!(i-1)
         y = ys!!(j-1)

--Optimized version of the above function. Also uses a table
optAlignments' :: String -> String -> [AlignmentType]
optAlignments' xs ys = snd $ optAlign (length xs) (length ys)
  where
    optAlign i j = optTable!!i!!j
    optTable = [[ optEntry i j | j <- [0..]] | i <-[0..] ]

    --Keeps track of the values in the table
    optEntry :: Int -> Int -> (Int, [AlignmentType])
    optEntry 0 0 = (0, [([],[])])
    optEntry i 0 = (scoreSpace + fst (optEntry (i-1) 0), attachTails (xs!!(i-1)) '-' (snd (optEntry (i-1) 0)))
    optEntry 0 j = (scoreSpace + fst (optEntry 0 (j-1)), attachTails '-' (ys!!(j-1)) (snd (optEntry 0 (j-1))))
    optEntry i j = (points, best) 
                
       where
         temp = [(fst (optAlign (i-1) (j-1)) + score x y, attachTails x y $ snd (optAlign (i-1) (j-1))),
                  (fst (optAlign (i-1) j) + scoreSpace, attachTails x '-' $ snd (optAlign (i-1) j)),
                  (fst (optAlign i (j-1)) + scoreSpace, attachTails '-' y $ snd (optAlign i (j-1)))]
 
         points = maximum $ map fst temp 
         best = concatMap snd [a | a <- temp, fst a == points] 

         x = xs!!(i-1)
         y = ys!!(j-1)

outputOptAlignments' string1 string2 = do 
 putStrLn $ "There are " ++ show  (length result) ++ " optimal alignments:\n"
 putStrLn $ foldl1 (++) (map (\(x,y) -> x ++ "\n" ++ y ++ "\n\n") result) 
    where 
     result = optAlignments' string1 string2
