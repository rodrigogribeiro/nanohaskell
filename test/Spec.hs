{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.State

import Test.Tasty
import Test.Tasty.HUnit
import qualified Text.Parsec as P hiding (State)
import Text.Parsec.Indent    

import Parser.NanoHaskellParser
import Parser.CoreParser    

import Syntax.Name    
import Syntax.NanoHaskell    

import Utils.Pretty    

   
main :: IO ()
main = defaultMain tests

tests :: TestTree       
tests = testGroup "Tests" [parserPrettyTests]
       
parserPrettyTests :: TestTree
parserPrettyTests = testGroup "Parser and Pretty Printer Tests"
                              [
                                testGroup "Literals" [ testCase "Char:" $ genSyntaxPrettyTest (LitChar 'c') charP
                                                     , testCase "String:" $ genSyntaxPrettyTest (LitString "abc") stringP
                                                     , testGroup "Numbers:"
                                                                 [ testCase "Int positive:" $ genSyntaxPrettyTest (LitInt 23) numberP
                                                                 , testCase "Int negative:" $ genSyntaxPrettyTest (LitInt (-23)) numberP
                                                                 , testCase "Float:" $ genSyntaxPrettyTest (LitFloat (23.45)) numberP  
                                                                 ]
                                                     ]
                              , testGroup "Expressions" [ testCase "Var:" $ genSyntaxPrettyTest (EVar (Name "xs")) atomP
                                                        , testCase "Con:" $ genSyntaxPrettyTest (ECon (Name "Cons")) atomP
                                                        , testCase "Lit:" $ genSyntaxPrettyTest (ELit (LitFloat (3.14))) atomP
                                                        , testCase "Lam:" $ genSyntaxPrettyTest (ELam (Name "x") (EVar (Name "x"))) exprP 
                                                        ]
                              ]


genSyntaxPrettyTest :: (Show a, Eq a, PPrint a) => a -> Parser a -> Assertion
genSyntaxPrettyTest x px = case runIndent "" $ P.runParserT px () "" (render $ pprint x) of
                               Left e -> undefined
                               Right x' -> x @=? x'


