> {-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}
   
Type syntax and its operations
==============================

> module Syntax.Type where

> import Data.Generics (Data,Typeable)
> import Data.String
  
> import Utils.Pretty
> import Utils.Name    


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


Pretty printting

> instance PPrint TyVar where
>     pprint = pprint . unVar

> instance PPrint TyCon where
>     pprint (AlgTyCon n) = pprint n
>     pprint (PrimTyCon n) = pprint n
      
> instance PPrint Type where
>     pprint (TVar v) = pprint v
>     pprint (TCon c) = pprint c
>     pprint (TApp l r)
>           | atomic l = pprint l <+> pprint r
>           | otherwise = parens (pprint l) <+> pprint r
>     pprint (TArrow l r)
>           | arrow l = parens (pprint l) <+> larrow <+> pprint r
>           | otherwise = pprint l <+> larrow <+> pprint r

> instance PPrint Kind where
>     pprint KStar = text "*"
>     pprint (KArr k k')
>            | arr k = parens (pprint k) <+> larrow <+> pprint k'    
>            | otherwise = pprint k <+> larrow <+> pprint k'
>     pprint (KVar n) = pprint n                     

> instance PPrint Pred where
>     pprint (IsIn n ts) = pprint n <+> pSpace ts
      
Wired-in types

> intTyCon :: TyCon
> intTyCon = PrimTyCon "Int"

> charTyCon :: TyCon
> charTyCon = PrimTyCon "Char"

> floatTyCon :: TyCon
> floatTyCon = PrimTyCon "Float"

> doubleTyCon :: TyCon
> doubleTyCon = PrimTyCon "Double"            


Auxiliar stuff

> preds :: [Pred] -> Doc
> preds ps
>    | null ps = empty
>    | otherwise = parens (pComma ps) <+> text "=>"
         
> atomic :: Type -> Bool
> atomic (TVar _) = True
> atomic (TCon _) = True
> atomic _  = False                  

> arrow :: Type -> Bool
> arrow (TArrow _ _) = True
> arrow _ = False                     

> arr :: Kind -> Bool
> arr (KArr _ _) = True
> arr _ = False                   
