{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Homework3 where
import Test.Hspec
import RPNAST

type RPNResult2    = Result String String


--prob1    :: a
--prob1    = undefined
-- We used the ideas we discussed in class to complete probl1.
-- The problem was entering a string, removing the white space, pattern matching on the x, adding the associated value to a list and returning the list. In order to get around the white space, we  used the words function and sent the result to the 'decipher' helper function to do the pattern matching.
--David
prob1 :: String -> PExp
prob1 x = prob1'(words x)
	where   prob1' :: [String] -> PExp	--helper function
		prob1' ("+":xs) = Plus:(prob1' xs)
 		prob1' ("-":xs) = Minus:(prob1' xs)
		prob1' ("*":xs) = Mul:(prob1' xs)
		prob1' ("/":xs) = IntDiv:(prob1' xs)
		prob1' (x:xs)   = Val(read x :: Int):(prob1' xs)
		prob1' [] 	= []

-- To solve problem 2, we used the RPN example from "Learn You A Haskell for the Greater Good".
-- The problem involves taking in a list, pattern matchin on each item. If the item is a value, add it to the list. If the item is an operation, foldr the operation over the list.
--David

prob2 :: PExp -> Int
prob2 ((Val x):[])	= x --if theres just a single number, return it
prob2 (x:[])		= errorWithoutStackTrace "***Exception: Bad Input."
prob2 a = prob2' a []
	where
		prob2' ((Val x):xs) stack		= prob2' xs (x:stack)
		prob2' ((Plus):xs) stack 		= prob2' xs ((foldr (+) 0 stack):[])
		prob2' ((Minus):xs) stack		= prob2' xs ((foldr (-) 0 stack):[])
		prob2' ((Mul):xs) stack			= prob2' xs ((foldr (*) 1 stack):[])
		prob2' (IntDiv:xs) (x:y:stack)		= prob2' xs ((div y x):stack)
		prob2' [] [x]				=  x

-- For problem 3 I used an example, I found off reddit.com/r/haskell. The solution for prob2 didn't work for prob3 as the operation was only supposed to work on the last 2 operations in the list
-- so now when it found an operation, it looked at the last two variables, performed the operation against them, and added that number to the list.
--	where foldingFunction (x:y:ys) "+" = (y + x):ys
--	        foldingFunction (x:y:ys) "-" = (y - x):ys
--		foldingFunction (x:y:ys) "*" = (y * x):ys
--	        foldingFunction (x:y:ys) "/" = (y / x):ys
--		foldingFunction xs numberString = read numberString:xs
--David
prob3	:: PExp -> RPNResult
prob3 ((Val x):[])		= Success x --if theres just a single number, return it
prob3 (x:[])			= Failure (BadSyntax)
prob3 a	= prob3' a []
	where
		prob3' ((Val x):xs) stack		= prob3' xs (x:stack)
		prob3' ((Plus):xs) (x:y:stack) 		= prob3' xs ((x + y):stack)
		prob3' ((Minus):xs) (x:y:stack)		= prob3' xs ((y - x):stack)
		prob3' ((Mul):xs) (x:y:stack)		= prob3' xs ((x * y):stack)
		prob3' ((IntDiv):xs) (x:y:stack)
			| x == 0 = Failure(DivByZero)
			| x /= 0 = prob3' xs ((div y x):stack)
		prob3' [] [x]				= Success x
		prob3' _ []				= Failure (BadSyntax)






prob4	:: PExp  -> RPNResult2
prob4 ((Val x):[]) = Success (show x)
prob4 (_:[])       = Failure "Bad Input."
prob4 a = prob4' a [] []
	where
		prob4' :: PExp -> [Int] -> String -> RPNResult2
		prob4' ((Val x):xs) stack stack2     = prob4' xs (x:stack) stack2
		prob4' (Plus:xs) (x:y:stack) stack2      = prob4' xs (drop 2 stack) (stack2++"("++(show y)++" + "++(show x)++")")
		prob4' (Plus:xs) (x:stack) stack2        = prob4' xs (drop 1 stack) ("("++stack2++" + "++(show x)++")")
		prob4' (Minus:xs) (x:y:stack) stack2       = prob4' xs (drop 2 stack) (stack2++"("++(show y)++" - "++(show x)++")")
		prob4' (Minus:xs) (x:stack) stack2       = prob4' xs (drop 1 stack) ("("++stack2++" - "++(show x)++")")
		prob4' (Mul:xs) (x:y:stack) stack2        = prob4' xs (drop 2 stack) (stack2++"("++(show y)++" * "++(show x)++")")
		prob4' (Mul:xs) (x:stack) stack2        = prob4' xs (drop 1 stack) ("("++stack2++" * "++(show x)++")")
	        prob4' (IntDiv:xs) (x:y:stack) stack2    = prob4' xs (drop 2 stack) (stack2++"("++(show y)++" / "++(show x)++")")
	        prob4' (IntDiv:xs) (x:stack) stack2    = prob4' xs (drop 1 stack) ("("++stack2++" / "++(show x)++")")

  	   	prob4' [] [] stack2         = Success stack2
     	  	prob4' _ _ _          = Failure "Bad Input."


-- Write your Hspec Tests below

--For test cases I used the information put out in class and the test cases from prior classes.
-- Unfortunately, I was unable to find out how to test the exceptions thrown in prob2 in the time allotted.
--David
--Dont use this technique for prob3
--error_example :: IO()
--error_example = hspec $ do
--  describe "Eval" $ do
--    context "For division by 0" $ do
--      it "should return an error" $ do
--        evaluate (eval [Val 1,Val 0,IntDiv]) `shouldThrow` anyExceptio
--
test_prob1 :: IO ()
test_prob1 = hspec $ do
	describe "prob1 test case" $ do
		it "200 + - * / should return [Val 200, Plus, Minus, Mul, IntDiv]" $ do
			prob1 "200 + - * /" `shouldBe` [Val 200, Plus, Minus, Mul, IntDiv]
		it "+ - * / 200 should return [Plus, Minus, Mul, IntDiv, Val 200]" $ do
			prob1 "+ - * / 200" `shouldBe` [Plus, Minus, Mul, IntDiv, Val 200]

test_prob2 ::IO ()
test_prob2 = hspec $ do
	describe "prob2 test case" $ do
		it "[Val 4, Val 2, IntDiv] should return 2" $ do
			prob2 [Val 4, Val 2, IntDiv] `shouldBe` 2


test_prob3 :: IO ()
test_prob3 = hspec $ do
	describe "prob3 test case" $ do
		it "[Val 5, Val 0, IntDiv] should return Failure DivByZero" $ do
			prob3 [Val 5, Val 0, IntDiv] `shouldBe` Failure DivByZero
		it "prob3 [IntDiv, Plus, Val 0] should return Failure InvalidInput" $ do
			prob3 [IntDiv, Plus, Val 0] `shouldBe` Failure BadSyntax
		it "prob3 [Val 5, Val 1, Val 1, Plus, Mul] should return Success 10" $ do
			prob3 [Val 5, Val 1, Val 1, Plus, Mul] `shouldBe` Success 10

test_prob4 :: IO ()
test_prob4 = hspec $ do
	describe "prob4 test case" $ do
		it "[Val 1, Val 1, Plus] should return Success '(1 + 1)'" $ do
			prob4 [Val 1, Val 1, Plus] `shouldBe` Success "(1 + 1)"
		it "[Val 2, Val 4, Plus, Val 3, IntDiv] should return Success '((2 + 4) / 3'" $ do
			prob4 [Val 2, Val 4, Plus, Val 3, IntDiv] `shouldBe` Success "((2 + 4) / 3)"
		it "[Val 2] should return Success '2'" $ do
			prob4 [Val 2] `shouldBe` Success "2"
		it "[Plus] should return Failure 'Bad Input.'" $ do
			prob4 [Plus] `shouldBe` Failure "Bad Input."
