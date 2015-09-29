> {-# LANGUAGE DeriveDataTypeable #-}
   
Nano Haskell Abstract Syntax Tree
======================

> module Syntax.NanoHaskell where

> import Data.Generics  

Literal syntax

> data Literal = LitInt Int
>              | LitChar Char
>              | LitFloat Float
>              | LitString String
>              deriving (Eq, Ord, Show, Data,Typeable)


Expressions

> data Expr = EVar Name            
>           | ECon Name            
>           | ELit Literal         
>           | ELam Name Expr
>           | EApp Expr Expr
>           | ELet Name Expr Expr
>           | EIf Expr Expr Expr
>           | ECase Expr [Match]
>           | EAnn Expr Type
>           | EDo [Stmt]
>           | EFail
>           deriving (Eq, Ord, Show, Data, Typeable)
            
Do statements

> data Stmt = Generator Pattern Expr
>           | Qualifier Expr
>           deriving (Eq, Ord, Show, Data, Typeable)

Patterns

> data Pattern = PVar Name
>              | PCon Name [Pattern]
>              | PLit Literal
>              | PWild
>              deriving (Eq, Ord, Show, Data, Typeable)


Equations
            
> data BindGroup = BindGroup {
>                     matchName  :: Name
>                  ,  matchPats  :: [Match]
>                  ,  matchType  :: Maybe Type
>                  ,  matchWhere :: [[Decl]]
>                  } deriving (Eq, Ord, Show, Data, Typeable)                    

> data Match = Match {
>                matchPat  :: [Pattern]
>              , matchBody :: Expr              
>              } deriving (Eq, Ord, Show, Data, Typeable)              

Declarations

> data ConDecl = ConDecl Constr Type 
>              | RecDecl Constr [(Name, Type)] Type
>                deriving (Eq, Ord, Show, Data, Typeable)

> data Decl = FunDecl BindGroup      
>           | TypeDecl Type          
>           | DataDecl Constr [Name] [ConDecl]  
>           | ClassDecl [Pred] Name [Name] [Decl]
>           | InstDecl [Pred] Name Type [Decl]   
>           | FixityDecl FixitySpec                
>             deriving (Eq, Ord, Show, Data, Typeable)

> data FixitySpec = FixitySpec {
>                     fixityFix :: Fixity
>                   , fixityName :: String
>                   } deriving (Eq, Ord, Show, Data, Typeable)

> data Assoc
>   = L
>   | R
>   | N
>  deriving (Eq, Ord, Show, Data, Typeable)


> data Fixity = Infix Assoc Int
>             | Prefix Int
>             | Postfix Int
>             deriving (Eq, Ord, Show, Data, Typeable)

> data Module = Module Name [Decl]
>               deriving (Eq, Ord, Show, Data, Typeable)
                                               

Pretty printting instances

> instance PPrint Literal where
>     pprint (LitInt i) = integer i
>     pprint (LitChar c) = char c
>     pprint (LitFloat f) = float f
>     pprint (LitString s) = doubleQuotes (text s)


> instance PPrint Expr where
>     pprint (EVar n) = pprint n
>     pprint (ECon n) = pprint n
>     pprint (ELit l) = pprint l
>     pprint (ELam n e) = hcat [slash, pprint n, arrow, pprint e]
>     pprint (EApp l r)
>            | isBase l  = parens (pprint l) <+> pprint r
>            | otherwise = pprint l <+> pprint r
>     pprint (ELet n e e') = llet <> nl <> nest 3 (pprint n <> eq <> pprint e)
>                                 <> nl <> lin <> pprint e'
>     pprint (ECase e ms) = lcase <+> pprint e <+> lof <> nl <> pLines ms
>     pprint (EAnn e t) = pprint e <+> colon <+> pprint t
>     pprint (EIf e e' e'') = lif <+> pprint e <+> lthen <+> pprint e'
>                                 <+> lelse    <+> pprint e''
>     pprint (EDo ss) = ldo <> nl <> nest 3 (pSemi ss)
>     pprint EFail = empty

> instance PPrint Pattern where
>     pprint (PVar n) = pprint n
>     pprint (PCon n ps) = parens (pprint n <+> hsep (map pprint ps))
>     pprint (PLit l) = pprint l
>     pprint PWild = text "_"

> instance PPrint Stmt where
>     pprint (Generator p e) = pprint p <+> rarrow <+> pprint e
>     pprint (Qualifier e) = pprint e                         

> instance PPrint Match where
>     pprint (Match ps e) = hsep (map pprint ps) <+> eq <+> pprint e

> instance PPrint BindGroup where
>     pprint (BindGroup n ms t ws) = t' <> ms' <> ws'
>                            where
>                              n' = pprint n
>                              ms' = punct nl $ map ((pprint n <+>) . pprint) ms
>                              t' = maybe empty ((<> nl) . pprint) t
>                              ws' = if null ws then empty
>                                    else lwhere <+> nl <> nest 3 (pLines (concat ws))
