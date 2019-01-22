-- implementation based on BST

module Set (
    Set (..),   -- instance Eq, Show
    null,
    member,
    notMember,
    isSubsetOf,
    insert,
    union,
    fromList,
    toList,
) where

import Prelude hiding (null)

data Set a = Empty | Tree a (Set a) (Set a)

null :: Set a -> Bool
null Empty = True
null _ = False

member :: (Ord a) => a -> Set a -> Bool
member _ Empty = False
member value (Tree root left right) =
    case compare value root of
        EQ -> True
        LT -> member value left
        GT -> member value right

notMember :: (Ord a) => a -> Set a -> Bool
notMember value tree = not $ member value tree

isSubsetOf :: (Ord a) => Set a -> Set a -> Bool
isSubsetOf Empty _ = True
isSubsetOf _ Empty = False
isSubsetOf (Tree root left right) tree =
    (member root tree) && (isSubsetOf left tree) && (isSubsetOf right tree)

insert :: (Ord a) => a -> Set a -> Set a
insert value Empty = (Tree value (Empty) (Empty))
insert value (Tree root left right) =
    case compare value root of
        EQ -> (Tree value left right)
        LT -> (Tree root (insert value left) right)
        GT -> (Tree root left (insert value right))

union :: (Ord a) => Set a -> Set a -> Set a
union tree Empty = tree
union Empty tree = tree
union tree (Tree root left right) =
    union (union (insert root tree) left) right

fromList :: (Ord a) => [a] -> Set a
fromList [] = Empty
fromList (x:xs) = insert x $ fromList xs

toList :: Set a -> [a]
toList Empty = []
toList (Tree root left right) = (toList left) ++ [root] ++ (toList right)

instance Eq a => Eq (Set a) where
    tree1 == tree2 = toList tree1 == toList tree2

instance Show a => Show (Set a) where
    show = show.toList
