> {-# LANGUAGE DeriveDataTypeable #-}
   
Nano Haskell Abstract Syntax Tree
======================

> module Syntax.NanoHaskell where

> import Data.Generics (Data,Typeable) 

> import Syntax.Name  
> import Syntax.Type  
  
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
                                               

