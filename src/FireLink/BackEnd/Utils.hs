module FireLink.BackEnd.Utils where

-- | Polimorfic implementation for fixedPoint algorithms
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint = until =<< ((==) =<<)

-- | Given a list, returns a list of pairs in which
-- | each first component is an unique integer, and each second
-- | component is a (now uniquely identified) element of the first list,
-- | in the original order
numberList :: [a] -> [(Int, a)]
numberList = zip [0..]
