module LinkedList where

data List a = Nil | Cons a (List a) deriving (Show, Eq)

datum Nil        = error "empty list"
datum (Cons a _) = a

fromList :: [a] -> List a
fromList = foldr Cons Nil

isNil :: List a -> Bool
isNil Nil = True
isNil _   = False

next :: List a -> List a
next Nil = Nil
next (Cons _ b) = b

new :: a -> List a -> List a
new = Cons

nil :: List a
nil = Nil

reverseLinkedList :: List a -> List a
reverseLinkedList Nil = Nil
reverseLinkedList (Cons a b) =
    append (reverseLinkedList b) (Cons a Nil)

toList :: List a -> [a]
toList Nil = []
toList (Cons a b) = a : toList b

-- I couldn't see a nice way to reverse a list without this (unless i did snoc
-- lists and translated between list types!)
append Nil a = a
append a Nil = a
append (Cons a b) (Cons c d) =
    Cons a (append b (Cons c d))
