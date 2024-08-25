module Helpers.Tree where

data Tree a = Tree a [Tree a]

find_in_children :: (a -> Bool) -> [Tree a] -> Maybe a
find_in_children f ((Tree value []):xs) | f value   = Just value
                                        | otherwise = find_in_children f xs
find_in_children f ((Tree value children):xs) | f value = Just value
                                              | otherwise = find_in_children f children
find_in_children _ [] = Nothing

find_in_tree :: (a -> Bool) -> Tree a -> Maybe a
find_in_tree f (Tree value children) | f value   = Just value
                                     | otherwise = find_in_children f children