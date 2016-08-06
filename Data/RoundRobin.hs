module Data.RoundRobin
    ( RoundRobin
    , newRoundRobin
    , select
    , set
    ) where

import Data.IORef
import Data.List.NonEmpty (NonEmpty(..))

-- | A simple round-robin table
-- useful for selecting resource using round-robin fashion.
newtype RoundRobin a = RoundRobin (IORef [a]) deriving Eq

-- | create a round-robin table from list.
--
-- If list is empty, an error will be raised.
-- will use 'NonEmpty' in future(ghc 8 are widely used).
newRoundRobin :: NonEmpty a -> IO (RoundRobin a)
newRoundRobin (x :| xs) =
    newIORef (cycle (x:xs)) >>= return . RoundRobin

-- | select an item from round-robin table.
select :: RoundRobin a -> IO a
select (RoundRobin ref) = atomicModifyIORef' ref (\ (a:as) -> (as, a))

-- | set a new round-robin table.
set :: RoundRobin a -> NonEmpty a -> IO ()
set (RoundRobin ref) (x :| xs) = atomicModifyIORef' ref (const (cycle (x:xs) , ()))
