{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Representations.Eval.Model where


newtype Entity = A { getInt :: Int } deriving (Show, Eq, Enum)

entities :: [Entity]
entities = [A 0..A 100]

-- | Individuals

ca, m, a, v :: Entity
ca = A 100
m = A 101
a = A 102
v = A 103

-- | Properties

human' :: Entity -> Bool
human' x = x `elem` [A 5..A 50]

dog' :: Entity -> Bool
dog' x = x == A 1 || x == A 2

cat' :: Entity -> Bool
cat' x = x == A 3 || x == A 4

happy' :: Entity -> Bool
happy' x = x == A 1 || x == A 3

-- | Binary relations

chase' :: Entity -> Entity -> Bool
chase' x y = (y == A 1 && (x == A 3 || x == A 4)) || (y == A 2 && x == A 4)

catch' :: Entity -> Entity -> Bool
catch' x y = (y == A 1 && x == A 4) || (y == A 2 && x == A 4)

-- | Measures

height' :: Entity -> Double
height' x = case x of
              (A 10) -> 75
              (A 11) -> 70
              (A 12) -> 69
              (A 13) -> 64
              (A 14) -> 66
              _ -> 69

height'' :: Entity -> Double
height'' x = case x of
               (A 100) -> 65
               (A 101) -> 67
               (A 102) -> 72
               (A 103) -> 68
