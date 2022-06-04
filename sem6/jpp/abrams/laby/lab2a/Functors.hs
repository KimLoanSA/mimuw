module Functors where

import           Prelude hiding (Functor, Just, Maybe, Nothing, fmap, map,
                          (<$>))

-- Wiemy, już że każdą funkcję

-- f :: a -> b

-- możemy rozszerzyć do funkcji

-- map f :: [a] -> [b]

-- w taki sposób, że

-- map f . map g = map (f . g)


-- Na rozgrzewkę napiszmy  tę definicję

map :: (a -> b) -> [a] -> [b] -- drugie nawiasy można zwykle pominąć
map f []     = []
map f (x:xs) = f x : map f xs


-- O każdym typie danych, który umożliwia takie rozszerzanie funkcji,
-- powiemy że jest funktorem

class Functor f where
    fmap :: (a -> b) -> (f a -> f b)
    -- dodadkowo zakładamy, że
    -- fmap f . fmap g = fmap (f . g)

instance Functor [] where
    fmap = map


-- Przypomnijmy definicję Maybe
data Maybe a = Nothing | Just a

-- (jest to prawie to samo co lista, która z definicji jest pusta lub jednoelementowa)
-- Maybe też jest funktorem:

instance Functor Maybe where
    fmap _ Nothing = Nothing
    fmap f Just x  = Just $ f x


-- O liście [a] można myśleć jak o specjanym przypadku (częściowej) funkcji

-- Int -> a

-- pokażmy, że dowolna tego typu funkcja jest funtorem

newtype IntFunction a = IntFunction (Int -> a)

instance Functor IntFunction where
    fmap f (IntFunction gcd) = IntFunction $ undefined

-- Co wiecej, dla każdego b funkcje b -> a są funktorami

-- Co wiecej, dla każdego b funkcje b -> a są funktorami

newtype Reader b a = Reader (b -> a)

instance Functor (Reader b) where -- częściowa aplikacja konstruktora typów
    fmap f (Reader g) = Reader $ \x -> f (g x)


-- Zdefiniujmy drzewo

data Tree a = Empty | Node (Tree a) a (Tree a)

-- Można o nim myśleć jak o liście ''z kształtem''.
-- Drzewo też jest funktorem

instance Functor Tree where
    fmap f Empty        = Empty
    fmap f (Node l v r) = Node (fmap f l) (f v) (fmap f r)

-- Praktyczny aspekt funtorów jest taki, że w prelude zdefiniowane jest

(<$>) = fmap

-- W związku z tym, żeby napisać funkcję

mul2Maybe :: Maybe Int -> Maybe Int
mul2Maybe Nothing  = Nothing
mul2Maybe (Just x) = Just (2*x)

-- wystarczy napisać

mul2Maybe' x = (2*) <$> x

-- albo, w tym przypadku

mul2Maybe'' = fmap (2*)
