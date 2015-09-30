import Test.Tasty
import Test.Tasty.HUnit
import Text.Parsec

    
import Parser.NanoHaskellParser
    
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
                              ]





genSyntaxPrettyTest :: (Show a, Eq a, PPrint a) => a -> Parser a -> Assertion
genSyntaxPrettyTest x px = case parse px "" (render $ pprint x) of
                               Left e -> undefined
                               Right x' -> x @=? x'
