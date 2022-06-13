module List where

data List a = Empty | Entry a (List a)
  deriving Eq
infixr `Entry`

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry x xs) = Entry (f x) (mapList f xs)

appendList :: List a -> List a -> List a
appendList Empty list2 = list2
appendList (Entry x xs) list2 = Entry x (appendList xs list2)

change :: Eq a => a -> a -> List a -> List a
change xOld xNew Empty = Empty
change xOld xNew (Entry x xs) = if x == xOld
    then Entry xNew xs 
    else Entry x (change xOld xNew xs)

changeAll :: Eq a => a -> a -> List a -> List a
changeAll xOld xNew Empty = Empty
changeAll xOld xNew (Entry x xs) = if x == xOld
    then Entry xNew (change xOld xNew xs) 
    else Entry x (change xOld xNew xs)

find :: Eq a => List a -> a -> Bool
find xs xToFind = not (allList (mapList not (mapList (== xToFind) xs)))

allList :: List Bool -> Bool
allList Empty = True
allList (Entry False bs) = False
allList (Entry True bs) = allList bs

listElement :: List a -> Integer -> a
listElement list i = help list 0 i
  where
    help :: List a -> Integer -> Integer -> a
    help (Entry x Empty) j i = x
    help (Entry x xs) j i 
      | j == i = x
      | otherwise = help xs (j + 1) i