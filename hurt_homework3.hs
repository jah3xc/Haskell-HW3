{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Homework3 where
import Test.Hspec
import RPNAST


--prob1    :: a
--prob1    = undefined
prob1 :: String -> PExp
prob1 x = decipher(words x)

decipher :: [String] -> PExp
decipher ("+":xs) = Plus:(decipher xs) 
decipher ("-":xs) = Minus:(decipher xs)
decipher ("*":xs) = Mul:(decipher xs)
decipher ("/":xs) = IntDiv:(decipher xs)
decipher (x:xs)   = Val(read x :: Int):(decipher xs)
decipher [] = []

prob2    :: a
prob2    = undefined
--prob2 [val 5, val 4, Mul]	Stack 1) Val 5 2) Val 4 3) Val 5 * Val 4 4) [Val 20]
--prob2 []			
--Last case should be wild cards and error
-- look out for subtraction by 0

prob3    :: a
prob3    = undefined

prob4    :: a
prob4    = undefined

-- Write your Hspec Tests below
