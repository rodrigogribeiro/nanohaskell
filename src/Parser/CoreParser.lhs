Core Parser Definitions
==============

> module Parser.CoreParser where

> import Control.Monad.State
  
> import Data.Char           

> import GHC.Float

> import Text.Parsec hiding (State)
> import Text.Parsec.Indent
> import Text.Parsec.Pos (SourcePos)    
> import qualified Text.Parsec.Token as Tk

> import Syntax.NanoHaskell
> import Syntax.Name    

A type for parsers
   
> type Parser a = ParsecT String () (State SourcePos) a
       

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

> reservedOp :: String -> Parser ()
> reservedOp = Tk.reservedOp lexer

> parens :: Parser a -> Parser a
> parens = Tk.parens lexer
  
> lexer :: Tk.GenTokenParser String st (State SourcePos)  
> lexer = Tk.makeTokenParser haskellDef         
  
> haskellDef :: Tk.GenLanguageDef String st (State SourcePos)
> haskellDef = Tk.LanguageDef {
>                 Tk.commentStart   = "{-"
>               , Tk.commentEnd     = "-}"
>               , Tk.commentLine    = "--"
>               , Tk.nestedComments = True
>               , Tk.identStart     = letter
>               , Tk.identLetter = alphaNum <|> oneOf "_'"
>               , Tk.opStart	 = oneOf ":!#$%&*+./<=>?@\\^|-~"
>               , Tk.opLetter	 = oneOf ":!#$%&*+./<=>?@\\^|-~"
>               , Tk.reservedOpNames= []
>               , Tk.reservedNames  = []
>               , Tk.caseSensitive  = True                
>              }              
  
