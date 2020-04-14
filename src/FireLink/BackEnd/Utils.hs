module FireLink.BackEnd.Utils where

import           Data.Set as Set

-- | Polimorfic implementation for fixedPoint algorithms
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint = until =<< ((==) =<<)

-- | Given a list, returns a list of pairs in which
-- | each first component is an unique integer, and each second
-- | component is a (now uniquely identified) element of the first list,
-- | in the original order
numberList :: [a] -> [(Int, a)]
numberList = zip [0..]


-- | List of available registers for MIPS32
availableRegisters :: Set.Set Int
availableRegisters = Set.fromList [4 .. 25]
