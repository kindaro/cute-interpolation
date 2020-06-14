{-# language UnicodeSyntax #-}

module X where

import Data.String

class Function a where
  applyAfter :: a -> (String -> String) -> a

instance Function String where
  applyAfter f g = g f

instance (Function a, Show b) => Function (b -> a) where
  applyAfter f g = fmap (`applyAfter` g) f


(…) :: (Function a, Show s) => a -> String -> (s -> a)
(…) f x = \u -> f × (\s -> s ++ show u ++ x)

(×) :: Function a => a -> (String -> String) -> a
(×) = applyAfter
