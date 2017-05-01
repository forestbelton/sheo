module Language.Sheo.Printer (prettyPrint) where

import Language.Sheo.Types
import Text.PrettyPrint.ANSI.Leijen

prettyPrint :: Program -> Doc
prettyPrint = pretty

instance Pretty Program where
    pretty = printProgram

printProgram :: Program -> Doc
printProgram = vsep . map printDecl . programDecls
    where printDecl d@(DataDecl _ _)      = printDataDecl d
          printDecl s@(ServiceDecl _ _ _) = printServiceInterface s <$$> pretty (ServiceImpl s)

publicDefn :: [Doc] -> Doc -> [Doc] -> Doc
publicDefn anns decl cont = (vsep anns)
    <$$> (access "public" <+> decl)
    <+> (bracesBody1 cont)

printDataDecl :: Decl -> Doc
printDataDecl (DataDecl n fs) = publicDefn anns intr (map pretty fs)
    where anns = [ann "@Value.Immutable"]
          intr = text "interface" <+> pretty n

printServiceInterface :: Decl -> Doc
printServiceInterface d@(ServiceDecl n deps ms) = publicDefn [] intr defns
    where intr = text "interface" <+> interfaceName d
          defns = map (pretty . MethodDeclaration) ms

newtype ServiceImpl = ServiceImpl { getImpl :: Decl }
    deriving (Show)

instance Pretty ServiceImpl where
    pretty = printServiceImpl . getImpl

printServiceImpl :: Decl -> Doc
printServiceImpl d@(ServiceDecl n deps ms) = publicDefn [] decl defns
    where decl = text "class" <+> className d <+> text "implements" <+> interfaceName d
          fields = pretty (ImplDeps deps)
          cons = pretty (ImplCtor (className d) deps)
          defns = cons : map printServiceImplMethod ms

newtype ImplDeps = ImplDeps { getDeps :: [(Name, Maybe Ty)] }

instance Pretty ImplDeps where
    pretty = foldr (<>) empty . map go . getDeps
        where go (n, Just ty) = join space [ access "protected"
                                    , text "final"
                                    , pretty ty
                                    , pretty n <> semi
                                    ]

data ImplCtor = ImplCtor Doc [(Name, Maybe Ty)]

instance Pretty ImplCtor where
    pretty (ImplCtor n deps) = ann "@Inject"
        <$$> (access "public" <+> n <> (parens $ join (comma <> space) $ map depl deps))
        <+> (bracesBody $ map assign deps)
        where depl (n, Just ty) = pretty ty <+> pretty n
              assign (n, Just ty) = text "this." <> pretty n <+> equals <+> pretty n <> semi

ann :: String -> Doc
ann = green . text

access :: String -> Doc
access = yellow . text

bracesBody :: [Doc] -> Doc
bracesBody cs = white lbrace <> nest 2 body <$$> white rbrace
    where body = case cs of
                     [] -> empty
                     _  -> vsep cs

bracesBody1 :: [Doc] -> Doc
bracesBody1 cs = white lbrace <> nest 2 body <$$> white rbrace
    where body = case cs of
                     [] -> empty
                     _  -> foldr (<>) empty $ map ((line <> line) <>) cs

className :: Decl -> Doc
className decl = interfaceName decl <> text "Impl"

interfaceName :: Decl -> Doc
interfaceName (ServiceDecl n _ _) = pretty n <> text "Service"

join :: Doc -> [Doc] -> Doc
join _ []        = empty
join _ [h]       = h
join d (h:i:t)   = join d $ (h <> d <> i) : t

commaSep :: [Doc] -> Doc
commaSep = join (comma <> space)

newtype MethodSignature = MethodSignature { getSignature :: Method }

printMethodSignature :: Method -> Doc
printMethodSignature (Method r n ps _) = pretty r
    <+> (pretty n <> (parens $ commaSep $ map printParam ps))
    where printParam (Field n ty) = pretty ty <+> pretty n

instance Pretty MethodSignature where
    pretty = printMethodSignature . getSignature

newtype MethodDeclaration = MethodDeclaration { getDeclaration :: Method }

instance Pretty MethodDeclaration where
    pretty = (<> semi) . pretty . MethodSignature . getDeclaration

newtype MethodImplementation = MethodImplementation { getImplementation :: Method }

printServiceImplMethod :: Method -> Doc
printServiceImplMethod m@(Method _ _ _ ss) = (green $ text "@Override")
    <$$> (yellow $ text "public") <+> pretty (MethodSignature m)
    <+> bracesBody [empty, printStatements ss]

printStatements :: [Statement] -> Doc
printStatements [] = empty
printStatements xs = vsep $ map pretty t ++ [l']
    where l = last xs
          t = init xs
          l' = case l of
                   Simply expr -> text "return" <+> pretty expr <> semi
                   _ -> error "last statement in method must be a simple expr"

instance Pretty MethodImplementation where
    pretty = printServiceImplMethod . getImplementation

instance Pretty Statement where
    pretty (Assign n (Just ty) expr) = text "final" <+> pretty ty <+> pretty n <+> equals <+> pretty expr <> semi
    pretty (Simply expr) = pretty expr <> semi

instance Pretty Expr where
    pretty (I x) = int x
    pretty (B x) = text $ case x of
                              True  -> "true"
                              False -> "false"
    pretty (S s) = dquotes $ text s
    pretty (BOp op l r)  = pretty l <+> pretty op <+> pretty r
    pretty (Lam n e)     = parens (pretty n) <+> text "->" <+> pretty e
    pretty (App f x)     = pretty f <> parens (pretty x)
    pretty (FMap x f)    = pretty x <$$> nest 2 (dot <> text "map" <> parens (pretty f))
    pretty (Fold xs f x) = pretty xs <$$> nest 2 (dot <> text "reduce" <> parens (commaSep $ map pretty [x, f]))
    pretty (Var v _)     = pretty v
    pretty _             = empty

instance Pretty BinOp where
    pretty Add = text "+"

instance Pretty Field where
    pretty (Field n ty) = pretty ty <+> pretty n <> parens empty <> semi

instance Pretty Ty where
    pretty ty = case ty of
                TBool    -> text "Boolean"
                TInt     -> text "Integer"
                TString  -> text "String"
                TFun a b -> text "Function" <> langle <> pretty a <> comma <> pretty b <> rangle
                TList a  -> text "List" <> langle <> pretty a <> rangle
                TSet a   -> text "Set" <> langle <> pretty a <> rangle
                TClass c -> text c

instance Pretty Name where
    pretty = text . getName
