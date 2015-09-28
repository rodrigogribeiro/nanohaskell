A type class for pretty printting stuff
=======================

> module Utils.Pretty (module Text.PrettyPrint.HughesPJ,
>                                 PPrint(..)) where

> import Text.PrettyPrint.HughesPJ


> class PPrint a where
>     pprint :: a -> Doc


> pLines :: PPrint a => [a] -> Doc
> pLines = punct nl

> pSemi :: PPrint a => [a] -> Doc
> pSemi = punct semi         

> punct :: PPrint a => Doc -> [a] -> Doc
> punct d = foldr (\a ac -> pprint a <> d <> ac) empty 
      
> dot :: Doc
> dot = char '.'       

> slash :: Doc
> slash = char '\\'         

> arrow :: Doc
> arrow = text "->"

> rarrow :: Doc
> rarrow = text "<-"          

> llet :: Doc
> llet = text "let"

> lin :: Doc
> lin = text "in"       

> lcase :: Doc
> lcase = text "case"

> lof :: Doc
> lof = text "of"       

> ldo :: Doc
> ldo = text "do"

> nl :: Doc
> nl = char '\n'

> eq :: Doc
> eq = char '='

> lif :: Doc
> lif = text "if"

> lthen :: Doc
> lthen = text "then"         

> lelse :: Doc
> lelse = text "else"         
