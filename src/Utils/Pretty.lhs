A type class for pretty printting stuff
=======================

> module Utils.Pretty (module Text.PrettyPrint.HughesPJ,
>                      module Utils.Pretty) where

> import Text.PrettyPrint.HughesPJ


> class PPrint a where
>     pprint :: a -> Doc

> pSpace :: PPrint a => [a] -> Doc
> pSpace = foldr (\a ac -> pprint a <+> ac) empty        

> pLines :: PPrint a => [a] -> Doc
> pLines = punct nl

> pSemi :: PPrint a => [a] -> Doc
> pSemi = punct (semi <> nl)         

> pCommaNl :: PPrint a => [a] -> Doc
> pCommaNl = punct (comma <> nl)

> pComma :: PPrint a => [a] -> Doc
> pComma = punct comma
  
> punct :: PPrint a => Doc -> [a] -> Doc
> punct d = foldr (\a ac -> pprint a <> d <> ac) empty 
      
> dot :: Doc
> dot = char '.'       

> slash :: Doc
> slash = char '\\'         

> larrow :: Doc
> larrow = text "->"

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

> lwhere :: Doc
> lwhere = text "where"          

> ldata :: Doc
> ldata = text "data"         

> lclass :: Doc
> lclass = text "class"

> linst :: Doc
> linst = text "instance"

> linfix :: Doc
> linfix = text "infix"

> lbar :: Doc
> lbar = text "|"        
