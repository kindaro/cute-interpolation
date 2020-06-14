{-# language UnicodeSyntax, MultiParamTypeClasses #-}

module X where

import Data.String

class Function f s where
  applyAfter :: f -> (s -> s) -> f

instance Function s s where
  applyAfter f g = g f

instance Function a s => Function (b -> a) s where
  applyAfter f g = \x -> applyAfter (f x) g


(…) :: (Function a String, Show s) => a -> String -> (s -> a)
(…) f x = \u -> f × (\s -> s ++ show u ++ x)

(×) :: Function a String => a -> (String -> String) -> a
(×) = applyAfter
