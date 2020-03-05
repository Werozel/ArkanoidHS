module Service where

-- Function removes element from list and returns list without it
removeFromList :: (Eq a) => [a] -> a -> [a]
removeFromList (x:xs) el | x == el = removeFromList xs el
                         | otherwise = x: removeFromList xs el
removeFromList _ _ = []


elemInList :: (Eq a) => [a] -> a -> Bool
elemInList (x:xs) el | x == el = True
                     | otherwise = elemInList xs el
elemInList _ _ = False
