module Tree where

data Tree a = Tree a [Tree a]
  deriving (Show)

instance Functor Tree where
  -- fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Tree a xs) = Tree (f a) $ map (fmap f) xs

instance Applicative Tree where
  pure a = Tree a []
  (Tree a xs) <*> (Tree b ys) = Tree (a b) $ zipWith (<*>) xs ys

instance Monad Tree where
  return = pure
  (Tree x xs) >>= f = Tree y $ fmap (>>= f) xs ++ ys
   where
    (Tree y ys) = f x

findInChildren :: (a -> Bool) -> [Tree a] -> Maybe a
findInChildren f ((Tree value []) : xs)
  | f value = Just value
  | otherwise = findInChildren f xs
findInChildren f ((Tree value children) : xs)
  | f value = Just value
  | otherwise = findInChildren f children
findInChildren _ [] = Nothing

findInTree :: (a -> Bool) -> Tree a -> Maybe a
findInTree f (Tree value children)
  | f value = Just value
  | otherwise = findInChildren f children
