-- implementation based on BST

module Set (
    Set (..),   -- instance Eq, Show
    null,
    member,
    notMember,
    isSubsetOf,
    insert,
    delete,
    deleteMax,
    findMax,
    union,
    fromList,
    empty,
    toList,
    filter,
    map
) where

import Prelude hiding (null, filter, map)

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

delete :: (Ord a) => a -> Set a -> Set a
delete _ Empty = Empty
delete value (Tree root left right) =
    case compare value root of
        EQ -> fix left right
        LT -> (Tree root (delete value left) right)
        GT -> (Tree root left (delete value right))
    where
        fix left Empty = left
        fix Empty right = right
        fix left right = Tree (findMax left) (delete (findMax left) left) right

findMax :: (Ord a) => Set a -> a
findMax Empty = error "No maximal element"
findMax (Tree root _ right)
    | null right = root
    | otherwise = findMax right

deleteMax :: (Ord a) => Set a -> Set a
deleteMax Empty = Empty
deleteMax tree = delete (findMax tree) tree

union :: (Ord a) => Set a -> Set a -> Set a
union tree Empty = tree
union Empty tree = tree
union tree (Tree root left right) =
    union (union (insert root tree) left) right

empty :: Set a
empty = Empty

fromList :: (Ord a) => [a] -> Set a
fromList [] = Empty
fromList (x:xs) = insert x $ fromList xs

toList :: Set a -> [a]
toList Empty = []
toList (Tree root left right) = (toList left) ++ [root] ++ (toList right)

filter :: (Ord a) => (a -> Bool) -> Set a -> Set a
filter f = fromList.(filter' f).toList
    where
        filter' f [] = []
        filter' f (x:xs)
            | f x = x:(filter' f xs)
            | otherwise = filter' f xs

map :: (Ord b) => (a -> b) -> Set a -> Set b
map f = fromList.(map' f).toList
    where
        map' f [] = []
        map' f (x:xs) = (f x):(map' f xs)

instance Eq a => Eq (Set a) where
    tree1 == tree2 = toList tree1 == toList tree2

instance Show a => Show (Set a) where
    show = show.toList

instance Ord a => Ord (Set a) where
    compare tree1 tree2 = compare (toList tree1) (toList tree2)
