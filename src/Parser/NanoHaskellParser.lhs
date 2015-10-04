Simple parser for NanoHaskell
===================

> module Parser.NanoHaskellParser where

> import Control.Monad.State
  
> import Data.Char           

> import GHC.Float

> import Text.Parsec hiding (State)
> import Text.Parsec.Indent
> import Text.Parsec.Pos (SourcePos)    
> import qualified Text.Parsec.Token as Tk

> import Parser.CoreParser
    
> import Syntax.Name
> import Syntax.NanoHaskell
  


Expressions

> exprP :: Parser Expr
> exprP = flip ($) <$> appP <*> option id annP
>         where
>           annP = flip EAnn <$> (reservedOp "::" *> typeP)
        
> appP :: Parser Expr
> appP = foldl1 EApp <$> many1 atomP

> atomP :: Parser Expr
> atomP = choice [ ELit <$> literalP
>                , lamP 
>                , letP  
>                , ifP
>                , caseP
>                , doP
>                , parens exprP
>                , varOrConP  
>                , failP ]  

> lamP :: Parser Expr
> lamP = f <$> reservedOp "\\" <*> nameP <*> reservedOp "->" <*> exprP
>        where
>          f _ n _ e = ELam n e

> lbindP :: Parser LocalBind
> lbindP = Local <$> nameP <*> (symbol "=" *> exprP)
           
> letP :: Parser Expr            
> letP = f <$> reserved "let" <*> block lbindP <*> reserved "in"
>                             <*> exprP
>        where
>          f _ bs _ e = ELet bs e
>        
> ifP :: Parser Expr            
> ifP = f <$> reserved "if" <*> exprP <*> reserved "then"
>                           <*> exprP <*> reserved "else"
>                           <*> exprP
>       where
>         f _ e _ e' _ e'' = EIf e e' e''

> caseP :: Parser Expr          
> caseP = f <$> reserved "case" <*> exprP <*> reserved "of" <*>
>               block matchP
>         where
>           f _ e _ ms = ECase e ms

> matchP :: Parser Match
> matchP = f <$> many1 patternP <*> symbol "->" <*> exprP
>          where
>            f ps _ e = Match ps e

> patternP :: Parser Pattern
> patternP = choice [ PWild <$ wildP
>                   , PLit  <$> literalP
>                   , pVarOrCon ]
>            where
>               wildP = symbol "_"
>               pVarOrCon = (f . unName) <$> nameP <*>
>                                            many patternP
>               f xss@(x:xs) ps
>                   | isUpper x = PCon (Name xss) ps
>                   | otherwise = PVar (Name xss)

> doP :: Parser Expr  
> doP = EDo <$> (reserved "do" *> block stmtP)

> failP :: Parser Expr  
> failP = EFail <$ symbol "<<fail>>"

> typeP = undefined
                 
> varOrConP :: Parser Expr
> varOrConP = (f . unName) <$> nameP
>             where
>                f xss@(x:xs) 
>                   | isUpper x = ECon (Name xss)
>                   | otherwise = EVar (Name xss)
    
> literalP :: Parser Literal
> literalP = choice [numberP , charP , stringP]

Statements

> stmtP :: Parser Stmt
> stmtP = generatorP <|> qualifierP
>         where
>           generatorP = (\p _ e -> Generator p e) <$> patternP
>                                                  <*> symbol "<-"
>                                                  <*> exprP
>           qualifierP = Qualifier <$> exprP
  
