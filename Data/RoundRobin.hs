module Data.RoundRobin where

import Data.IORef

-- | A simple round-robin table
-- useful for selecting resource using round-robin fashion.
newtype RoundRobin a = RoundRobin (IORef [a]) deriving Eq

-- | create a round-robin table from list.
--
-- If list is empty, an error will be raised.
-- will use 'NonEmpty' in future(ghc 8 are widely used).
newRoundRobin :: [a] -> IO (RoundRobin a)
newRoundRobin xs =
    if null xs
    then error "can't create an empty RoundRobin"
    else newIORef (cycle xs) >>= return . RoundRobin

-- | select an item from round-robin table.
select :: RoundRobin a -> IO a
select (RoundRobin ref) =
    atomicModifyIORef' ref (\ (a:as) -> (as, a))
