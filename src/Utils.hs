module Utils where

import Control.Monad (foldM)

foldM2 :: Monad m => (c -> a -> b -> m c) -> c -> [a] -> [b] -> m c
foldM2 f i xs ys = foldM (\c (a, b) -> f c a b) i (zip xs ys)
