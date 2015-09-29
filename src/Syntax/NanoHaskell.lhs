> {-# LANGUAGE DeriveDataTypeable #-}
   
Nano Haskell Abstract Syntax Tree
======================

> module Syntax.NanoHaskell where

> import Data.Generics (Data,Typeable) 

> import Syntax.Type  
> import Utils.Name
> import Utils.Pretty
  
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

> data ConDecl = ConDecl Name Type 
>              | RecDecl Name [(Name, Type)] Type
>                deriving (Eq, Ord, Show, Data, Typeable)

> data Decl = FunDecl BindGroup      
>           | TypeDecl Name Type          
>           | DataDecl Name [Name] [ConDecl]  
>           | ClassDecl [Pred] Name [Name] [Decl]
>           | InstDecl [Pred] Name [Type] [Decl]   
>           | FixityDecl FixitySpec                
>             deriving (Eq, Ord, Show, Data, Typeable)

> data FixitySpec = FixitySpec {
>                     fixityFix :: Fixity
>                   , fixityName :: Name
>                   } deriving (Eq, Ord, Show, Data, Typeable)

> data Assoc
>   = L
>   | R
>   | N
>  deriving (Eq, Ord, Show, Data, Typeable)


> data Fixity = Infix Assoc Int
>             deriving (Eq, Ord, Show, Data, Typeable)

> data Module = Module Name [Decl]
>               deriving (Eq, Ord, Show, Data, Typeable)
                                               

Pretty printting instances

> instance PPrint Literal where
>     pprint (LitInt i) = int i
>     pprint (LitChar c) = char c
>     pprint (LitFloat f) = float f
>     pprint (LitString s) = doubleQuotes (text s)


> instance PPrint Expr where
>     pprint (EVar n) = pprint n
>     pprint (ECon n) = pprint n
>     pprint (ELit l) = pprint l
>     pprint (ELam n e) = hcat [slash, pprint n, larrow, pprint e]
>     pprint (EApp l r) = pprint l <+> pprint r
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
>                              ms' = hcat $ punctuate nl $ map ((pprint n <+>) . pprint) ms
>                              t' = maybe empty ((<> nl) . pprint) t
>                              ws' = if null ws then empty
>                                    else lwhere <+> nl <> nest 3 (pLines (concat ws))

> instance PPrint ConDecl where
>     pprint (ConDecl n t) = pprint n <> hsep (map pprint (splitRight t))
>                            where
>                              splitRight (TArrow l r) = l : (splitRight r)
>                              splitRight (TForall _ _ t) = splitRight t
>                              splitRight t = [t]                             
>     pprint (RecDecl n fs _) = pprint n <> braces (nl <> nest 3 pComma')
>                               where
>                                  pComma' = hsep $ punctuate (comma <> nl) (map step fs)
>                                  step (n,t) = pprint n <+> colon <+> pprint t 

> instance PPrint Assoc where
>     pprint L = char 'l'
>     pprint R = char 'r'
>     pprint N = empty
                                   
> instance PPrint Fixity where
>     pprint (Infix a n) = linfix <> pprint a <+> int n
                                   
> instance PPrint FixitySpec where
>     pprint (FixitySpec f n) = pprint f <+> pprint n
                                                                        
> instance PPrint Decl where
>     pprint (FunDecl bg) = pprint bg
>     pprint (TypeDecl n t) = pprint n <+> colon <+> pprint t
>     pprint (ClassDecl ps n vs ds) = lclass <+> preds ps <+> pprint n <+> pSpace vs <+> lwhere
>                                            <> nl <> nest 3 (pLines ds)
>     pprint (InstDecl ps n ts ds) = linst <+> preds ps <+> pprint n <+> pSpace ts <+> lwhere
>                                            <> nl <> nest 3 (pLines ds)
>     pprint (FixityDecl f) = pprint f                                         
>     pprint (DataDecl n ns cs) = ldata <+> pprint n <+> pSpace ns <+> pConstr cs
>                                 where
>                                    pConstr [] = empty
>                                    pConstr (x:xs) = eq <+> pprint x <> nl <> pConstr' xs
>                                    pConstr' [] = empty
>                                    pConstr' (y:ys) = lbar <+> pprint y <> nl <> pConstr' ys              

