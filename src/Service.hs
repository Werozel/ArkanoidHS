module Service where

-- Removes element from list and returns list without it
removeFromList :: (Eq a) => [a] -> a -> [a]
removeFromList [] _ = []
removeFromList (x:xs) el | x == el = removeFromList xs el
                         | otherwise = x: removeFromList xs el


-- Checks if the element is in the list
elemInList :: (Eq a) => [a] -> a -> Bool
elemInList [] _ = False
elemInList (x:xs) el | x == el = True
                     | otherwise = elemInList xs el
