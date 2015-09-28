> {-# LANGUAGE DeriveDataTypeable #-}
        
A module with Names and its functions
=====================================

> module Utils.Name where

> import Data.Generics          

> import Utils.Pretty
  
A data type for names

> data Name = Name String          -- ^ simple names
>           | Gen String Integer   -- ^ compiler generated name
>           | Qual Name String     -- ^ qualified name, needed for future renaming
>           deriving (Eq, Ord, Show, Data, Typeable)


Name generation

> letters :: [String]
> letters = [1..] >>= flip replicateM ['a'..'z']

> genNames :: [Name]
> genNames = zipWith Gen letters [0..]            

Pretty printting

> instance Pretty Name where
>    pprint (Name s) = text s
>    pprint (Gen s i) = text s <> int i
>    pprint (Qual n s) = pprint n <> dot <> text s                   
