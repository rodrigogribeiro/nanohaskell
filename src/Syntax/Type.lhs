> {-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
   
Type syntax and its operations
==============================

> module Syntax.Type where

> import Data.Generics (Data,Typeable)
> import Data.String
  
> import Syntax.Name    


Type definition

> newtype TyVar = TyVar { unVar :: Name }
>                deriving (Eq, Ord, Show, Data, Typeable)

> data TyCon = AlgTyCon { unAlgTyCon :: Name }
>            | PrimTyCon { unPrimTyCon :: Name }
>            deriving (Eq, Ord, Show, Data, Typeable)


> data Pred = IsIn Name [Type]
>             deriving (Eq, Ord, Show, Data, Typeable)


> data Type = TVar TyVar
>           | TCon TyCon
>           | TApp Type Type
>           | TArrow Type Type
>           | TForall [Pred] [TyVar] Type
>           deriving (Eq, Ord, Show, Data, Typeable)

Kind definition

> data Kind = KStar
>           | KArr Kind Kind
>           | KVar Name
>           deriving (Eq, Ord, Show, Data, Typeable)

Overloaded strings

> instance IsString TyVar where
>     fromString = TyVar . fromString

      
Wired-in types

> intTyCon :: TyCon
> intTyCon = PrimTyCon "Int"

> charTyCon :: TyCon
> charTyCon = PrimTyCon "Char"

> floatTyCon :: TyCon
> floatTyCon = PrimTyCon "Float"

> doubleTyCon :: TyCon
> doubleTyCon = PrimTyCon "Double"            


