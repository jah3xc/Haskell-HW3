{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Homework3 where
import Test.Hspec
import RPNAST
import Control.Exception (evaluate)
import Debug.Trace (trace)

--prob1    :: a
--prob1    = undefined
-- We used the ideas we discussed in class to complete probl1.
-- The problem was entering a  
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


--Dont use this technique for prob3
error_example :: IO()
error_example = hspec $ do
  describe "Eval" $ do
    context "For division by 0" $ do
      it "should return an error" $ do
        evaluate (eval [Val 1,Val 0,IntDiv]) `shouldThrow` anyException


--stack example
push :: a -> [a] -> [a]
push x stack = x:stack

pop :: [a] -> (a, [a])
pop []     = error "Cannot pop an empty stack"
pop [x]    = (x,[])
pop (x:xs) = (x,xs)

top :: [a] -> a
top []     = error "Empty stack"
top (x:xs) = x

evalRPN ops = go' ops []
  where go' (Plus:rest) (r:l:vals) = go' rest ((l+r):vals)
        go' ((Val i):rest) vals    = go' rest(i:vals)
        go' (Mul:rest) (r:l:vals)  = undefined
        -- etc etc
        go' [] [i]                 = i
        go' _ _                    = undefined
