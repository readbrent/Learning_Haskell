module JoinList where

import Data.Monoid

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty a = a
(+++) a Empty = a
(+++) a b = Append ((tag a) <> (tag b)) a b

leftSize :: (Sized b, Monoid b) => JoinList b a -> Int
leftSize Empty = 0
leftSize x = getSize . size . tag $ x

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _ | i < 0 = Nothing
indexJ i (Append s _ _) | i >= (getSize . size $ s) = Nothing
indexJ i (Single s x) | i == 0 = Just x

indexJ i (Append _ left right) = 
    if i < left_size then indexJ i left
    else indexJ (i - left_size) right
    where
        left_size = leftSize left
indexJ i _ = Nothing