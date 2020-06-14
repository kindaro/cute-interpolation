{-# language UnicodeSyntax #-}

module X where

import Data.String

class Function a where
  applyAfter :: a -> (String -> String) -> a
  applyBefore :: (String -> String) -> a -> a

instance Function String where
  applyAfter f g = g f
  applyBefore g f = g f

instance (Function a, Show b) => Function (b -> a) where
  applyAfter f g = fmap (`applyAfter` g) f
  applyBefore g f = fmap (g `applyBefore`) f


infixr 1 …
(…) :: (Function a, Show s) => String -> a -> (s -> a)
(…) x f = \u -> (\s -> x ++ show u ++ s) ÷ f

(×) :: Function a => a -> (String -> String) -> a
(×) = applyAfter

(÷) ∷ Function a ⇒ (String → String) → a → a
(÷) = applyBefore
