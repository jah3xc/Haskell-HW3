{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Homework3 where
import Test.Hspec
import RPNAST


--prob1    :: a
--prob1    = undefined
prob1 :: String -> PExp
prob1 x = decipher(words x)

decipher :: String -> PExp
  | [] = []
  | x == "+" = Plus:(decipher xs)
  | x == "-" = Minus:(decipher xs)
  | x == "*" = Mul:(decipher xs)
  | x == "/" = IntDiv:(decipher xs)
  | x        = (Val (read x :: Int)):(decipher xs)

prob2    :: a
prob2    = undefined

prob3    :: a
prob3    = undefined

prob4    :: a
prob4    = undefined

-- Write your Hspec Tests below
