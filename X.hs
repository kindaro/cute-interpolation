{-# language UnicodeSyntax, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, TypeFamilies #-}

module X where

import Data.String

class Function a b c | a b → c, a c → b where
  (÷) ∷ a → b → c
  infixl 1 ÷

instance Function String String String where
   f ÷ g = f <> g

instance Function a b c ⇒ Function (m → a) b (m → c) where
  f ÷ g = \x → f x ÷ g

instance {-# overlappable #-} Function String b b ⇒ Function String (m → b) (m → b) where
  f ÷ g = \x → f ÷ g x
