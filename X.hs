{-# language UnicodeSyntax, MultiParamTypeClasses #-}

module X where

import Data.Functor.Contravariant
import Data.Profunctor

class Function a b where
  applyAfter :: a -> (b -> b) -> a
  (×) :: a -> (b -> b) -> a
  (×) = applyAfter

instance Function String String where
  applyAfter f g = g f

instance (Function a String) => Function (String -> a) String where
  applyAfter f g = lmap (g) f


(…) :: (Function a String, Show s) => a -> String -> (s -> a)
(…) f x = \u -> f × (\s -> s ++ show u ++ x)
