> {-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
   
Type syntax and its operations
==============================

> module Syntax.Type where

> import Utils.Pretty       


Type definition

> newtype TVar = TVar { unVar :: Name }
>                deriving (Eq, Ord, Show, Data, Typeable)

> data TyCon = AlgTyCon { unAlgTyCon :: Name }
>            | PrimTyCon { unPrimTyCon :: Name }
>            deriving (Eq, Ord, Show, Data, Typeable)


> data Pred = IsIn Name [Type]
>             deriving (Eq, Ord, Show, Data, Typeable)


> data Type = TVar TVar
>           | TCon TyCon
>           | TApp Type Type
>           | TArrow Type Type
>           | TForall [Pred] [TVar] Type
>           deriving (Eq, Ord, Show, Data, Typeable)

Kind definition

> data Kind = KStar
>           | KArr Kind Kind
>           | KVar Name
>           deriving (Eq, Ord, Show, Data, Typeable)

Overloaded strings

> instance IsString TVar where
>     fromString = TV . fromString


Wired-in types

> intTy :: Type
> intTy = PrimTyCon "Int"

> charTy :: Type
> charTy = PrimTyCon "Char"

> floatTy :: Type
> floatTy = PrimTyCon "Float"

> doubleTy :: Type
> doubleTy = PrimTyCon "Double"            
