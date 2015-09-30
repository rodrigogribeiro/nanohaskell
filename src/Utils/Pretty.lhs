A type class for pretty printting stuff
=======================

> module Utils.Pretty (module Text.PrettyPrint.HughesPJ,
>                      module Utils.Pretty) where

> import Text.PrettyPrint.HughesPJ

> import Syntax.Name
> import Syntax.NanoHaskell
> import Syntax.Type    

> class PPrint a where
>     pprint :: a -> Doc

> instance PPrint Name where
>    pprint (Name s) = text s
>    pprint (Gen s i) = text s <> int i
>    pprint (Qual n s) = pprint n <> dot <> text s                         

> instance PPrint Literal where
>     pprint (LitInt i) = int i
>     pprint (LitChar c) = quotes (char c)
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


> instance PPrint Module where
>     pprint (Module n ds) = lmodule <+> pprint n <+> lwhere <> nl <> pLines ds


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


            
Pretty printting functions
                     

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

Simple documents
   
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

> lmodule :: Doc
> lmodule = text "module"           

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
  
