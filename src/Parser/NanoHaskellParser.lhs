Simple parser for NanoHaskell
===================

> module Parser.NanoHaskellParser where

> import Data.Char          
> import Data.Functor
> import Data.Functor.Identity     

> import GHC.Float
        
> import Text.Parsec
> import Text.Parsec.Expr
> import Text.Parsec.Language    
> import qualified Text.Parsec.Token as Tk

> import Parser.LayoutCombinators
  
> import Syntax.Name
> import Syntax.NanoHaskell
  
A type for parsers
   
> type Parser a = IParsec a


> exprP :: Parser Expr
> exprP = chainl1 atomP (return EApp) 

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
> lamP = f <$> symbol "\\" <*> nameP <*> symbol "->" <*> exprP
>        where
>          f _ n _ e = ELam n e

> lbindP :: Parser LocalBind
> lbindP = Local <$> nameP <*> (symbol "=" *> exprP)
           
> letP :: Parser Expr            
> letP = f <$> reserved "let" <*> maybeBraces lbindP
>                             <*> reserved "in" <*> exprP
>        where
>           bindP = (,) <$> nameP <*> (symbol "=" *> exprP)
>           f _ lb _ e = ELet lb e

> ifP :: Parser Expr            
> ifP = f <$> reserved "if" <*> exprP <*> reserved "then"
>                           <*> exprP <*> reserved "else"
>                           <*> exprP
>       where
>         f _ e _ e' _ e'' = EIf e e' e''

> caseP :: Parser Expr          
> caseP = f <$> reserved "case" <*> exprP <*> reserved "of"
>                               <*> maybeBraces (matchP "->")
>         where
>           f _ e _ ms = ECase e ms

> matchP :: String -> Parser Match
> matchP sep = f <$> many1 patternP <*> symbol sep <*> exprP
>              where
>                f ps _ e = Match ps e

> patternP :: Parser Pattern
> patternP = undefined
                             
> annP = undefined

> doP = undefined
  
> failP = undefined 
                 
> varOrConP :: Parser Expr
> varOrConP = (f . unName) <$> nameP
>             where
>                f xss@(x:xs) 
>                   | isUpper x = ECon (Name xss)
>                   | otherwise = EVar (Name xss)
    
> literalP :: Parser Literal
> literalP = choice [numberP , charP , stringP]

> block :: Parser a -> Parser [a]
> block p = laidout (many (align >> p))         

> maybeBraces :: Parser a -> Parser [a]
> maybeBraces p = braces (endBy p semi) <|> block p
  
Building a haskell lexer

> charP :: Parser Literal
> charP = LitChar <$> charLiteral

> stringP :: Parser Literal
> stringP = LitString <$> stringLiteral           

> numberP :: Parser Literal
> numberP = (f <$> naturalOrFloat) <|> (g <$> (symbol "-" *> integer))
>           where
>             f = either (LitInt . fromInteger) (LitFloat . double2Float)
>             g = (LitInt . fromInteger . negate)
                 
> nameP :: Parser Name
> nameP = Name <$> identifier          

> identifier :: Parser String
> identifier = Tk.identifier lexer

> integer :: Parser Integer
> integer = Tk.integer lexer

> semi :: Parser ()
> semi = () <$ Tk.semi lexer        

> braces :: Parser a -> Parser a
> braces = Tk.braces lexer
  
> naturalOrFloat :: Parser (Either Integer Double)
> naturalOrFloat = Tk.naturalOrFloat lexer

> charLiteral :: Parser Char
> charLiteral = Tk.charLiteral lexer

> stringLiteral :: Parser String
> stringLiteral = Tk.stringLiteral lexer
  
> symbol :: String -> Parser String
> symbol = Tk.symbol lexer

> reserved :: String -> Parser ()
> reserved = Tk.reserved lexer 
  
> lexer :: Tk.TokenParser st
> lexer = Tk.makeTokenParser haskellDef         

  
