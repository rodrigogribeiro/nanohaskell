Simple parser for NanoHaskell
===================

> module Parser.NanoHaskellParser where

> import Data.Char          
> import Data.Functor

> import GHC.Float
        
> import Text.Parsec
> import Text.Parsec.Expr
> import Text.Parsec.Language    
> import Text.Parsec.Token

> import Syntax.Name
> import Syntax.NanoHaskell
  
A type for parsers
   
> type Parser a = ParsecT String () (State SourcePos) a


> exprP :: Parser Expr
> exprP = chainl1 (return EApp) atomP

> atomP :: Parser Expr
> atomP = choice [ varOrConP
>                , ELit <$> literalP
>                , lamP
>                , letP  
>                , ifP
>                , caseP
>                , annP
>                , doP
>                , failP ]  

> lamP :: Parser Expr
> lamP = ELam <$> symbol "\\" *> nameP <*> (symbol "->" *> exprP)

> letP = undefined

> ifP = undefined

> caseP = undefined

> annP = undefined

> doP = undefined
  
> failP = undefined 
                 
> varOrConP :: Parser Expr
> varOrConP = f <$> nameP
>             where
>                f (x:xs) 
>                   | isUpper x = ECon
>                   | otherwise = EVar
    
> literalP :: Parser Literal
> literalP = choice [numberP , charP , stringP]
  

Building a haskell lexer

> charP :: Parser Literal
> charP = LitChar <$> charLiteral lexer

> stringP :: Parser Literal
> stringP = LitString <$> stringLiteral lexer           

> numberP :: Parser Literal
> numberP = (f <$> naturalOrFloat lexer) <|> (g <$> (symbol lexer "-" *> integer lexer))
>           where
>             f = either (LitInt . fromInteger) (LitFloat . double2Float)
>             g = (LitInt . fromInteger . negate)
                 
> nameP :: Parser Name
> nameP = Name <$> identifier lexer         

> lexer :: TokenParser st
> lexer = makeTokenParser haskellDef         

  
