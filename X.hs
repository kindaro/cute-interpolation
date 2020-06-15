{-# language UnicodeSyntax, MultiParamTypeClasses, FunctionalDependencies, UndecidableInstances, TypeFamilies, DataKinds, KindSignatures, PolyKinds, TypeFamilyDependencies #-}

module X where

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

data Function k = Terminal k | Argument k (Function k)

type family Decode (f ∷ Function k) = (r ∷ *) where
  Decode (Terminal t) = t
  Decode (Argument t ts) = t → Decode ts

type family Encode (f ∷ *) = (r ∷ Function *) | r →f  where
  Encode (a → b) = Argument a (Encode b)
  Encode t = Terminal t

type family (f ∷ Function k) <> (g ∷ Function k) where
  Terminal t <> Terminal t = Terminal t
  Terminal t <> Argument a g = Argument a (Terminal t <> g)
  Argument a f <> g = Argument a (f <> g)

(…) ∷ f → g → Decode (Encode f <> Encode (s → String) <> Encode g)
f … g = _u
