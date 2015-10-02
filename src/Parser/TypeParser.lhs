Parsing type related things
=================

> module Parser.TypeParser where

> import Text.Parsec
> import Text.Parsec.Expr    

> import Syntax.Name
> import Syntax.Type    

toplevel type parser

> typeP :: Parser Type
> typeP = undefined         

type application
     
> tyAppP :: Parser Type
> tyAppP = chainl1 atomTyP (return TApp)
     
> atomTyP :: Parser Type
> atomTyP = choice [ tyVarOrTyConP
>                  , qualifiedTyP ]

> qualifiedTyP :: Parser Type
> qualifiedTyP = f <$> parens (predP `sepBy1` comma) <*> symbol "=>" <*> typeP
>                where
>                  f ps _ t = TForall ps [] t -- NEED to put here tv t

> predP :: Parser Pred
> predP = IsIn <$> nameP <*> many1 typeP
  
type variables and constructors

> tyVarOrTyConP :: Parser Type
> tyVarOrTyConP = (f . unName) <$> nameP
>                 where
>                   f xss@(x:xs)
>                       | isUpper x = TCon $ TyCon (Name xss)
>                       | otherwise = TVar $ TyVar (Name xss)              
