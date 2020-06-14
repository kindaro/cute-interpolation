{-# language UnicodeSyntax, UndecidableInstances, ScopedTypeVariables, MultiParamTypeClasses, NoMonomorphismRestriction #-}

module X where

class V v i where
  mapp ∷ v → [i] → v

instance Monoid i ⇒ V i i where
  mapp x xs = x `mappend` (mconcat xs)

instance V v i ⇒ V (v → v) i where
  mapp f xs = fmap (flip (mapp @v) xs) f

(÷) = mapp

(…) ∷ (Monoid i, V v i) ⇒ v → i → (i → v)
(…) f xs = \u → f ÷ [u `mappend` xs]

-- import Data.String

-- class Function a where
--   applyAfter ∷ a → (String → String) → a

-- instance Function String where
--   applyAfter f g = g f

-- instance (Function a, Show b) => Function (b → a) where
--   applyAfter f g = fmap (`applyAfter` g) f


-- (…) ∷ (Function a, Show s) => a → String → (s → a)
-- (…) f x = \u → f × (\s → s ++ show u ++ x)

-- (×) ∷ Function a => a → (String → String) → a
-- (×) = applyAfter

-- ("x" … "y" … "z") 10
-- (("x" … "y") … "z") 10
-- ((\ u → "x" × ( \ s → s ++ show u ++ "y") … "z") 10
-- ((\ u → ( "x" ++ show u ++ "y") … "z") 10
