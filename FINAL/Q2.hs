getPreyLists :: [(String, String)] -> [(String, [String])]
getPreyLists inputList = foldl getPreyHelper [(predator, prey)] (tail inputList) 
 where
 firstElem = head inputList
 predator = fst firstElem
 prey = [snd firstElem]


getPreyHelper :: [(String, [String])] -> (String, String) -> [(String, [String])]
getPreyHelper currentList (newPredator, newPrey)
  | currentPredator == newPredator = (init currentList) ++ [(currentPredator, currentPreyList ++ [newPrey])]
  | otherwise    =  currentList ++ [(newPredator, [newPrey])]

  where
  currentPredatorTuple = last currentList
  (currentPredator, currentPreyList) = currentPredatorTuple
