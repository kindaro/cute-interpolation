{-# language UnicodeSyntax, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, TypeFamilies, DataKinds, KindSignatures, PolyKinds, TypeFamilyDependencies, TypeOperators, TypeSynonymInstances, FlexibleInstances, FlexibleContexts, AllowAmbiguousTypes, PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Text.Cute.String where

import Data.String

class Group a b c | a b → c, a c → b where
  (÷) ∷ a → b → c
  infixl 1 ÷

instance Group String String String where
   f ÷ g = f <> g

instance Group a b c ⇒ Group (m → a) b (m → c) where
  f ÷ g = \x → f x ÷ g

instance {-# overlappable #-} Group String b b ⇒ Group String (m → b) (m → b) where
  f ÷ g = \x → f ÷ g x

type family (f ∷ *) <> (g ∷ *) where
  (a → f) <> g = a → (f <> g)
  a <> (b → g) = b → (a <> g)
  a <> a = a

(…) ∷ _ ⇒ f → g → f <> (s → String) <> g
f … g = f ÷ show ÷ g
