module Language.Sheo.Printer where

import Language.Sheo.Types
import Text.PrettyPrint.ANSI.Leijen

publicDefn :: [Doc] -> Doc -> [Doc] -> Doc
publicDefn anns decl cont = vsep $ anns ++
    [ text "public" <+> decl <+> lbrace
    , indent 2 (vsep $ map pnl cont)
    , rbrace
    ]
    where pnl l = line <> l

printDataDecl :: Decl -> Doc
printDataDecl (DataDecl n fs) = publicDefn anns intr (map pretty fs)
    where anns = [text "@Value.Immutable"]
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
          defns = fields : cons : map printServiceImplMethod ms

newtype ImplDeps = ImplDeps { getDeps :: [(Name, Maybe Ty)] }

instance Pretty ImplDeps where
    pretty = join (line <> line) . map go . getDeps
        where go (n, Just ty) = join space [ text "protected"
                                    , text "final"
                                    , pretty ty
                                    , pretty n <> semi
                                    ]

data ImplCtor = ImplCtor Doc [(Name, Maybe Ty)]

instance Pretty ImplCtor where
    pretty (ImplCtor n deps) = vsep
        [ text "@Inject"
        , text "public" <+> n <> (parens $ join (comma <> space) $ map depl deps) <+> lbrace
        , indent 2 (vsep $ map assign deps)
        , rbrace
        ]
        where depl (n, Just ty) = pretty ty <+> pretty n
              assign (n, Just ty) = text "this." <> pretty n <+> equals <+> pretty n <> semi

className :: Decl -> Doc
className decl = interfaceName decl <> text "Impl"

interfaceName :: Decl -> Doc
interfaceName (ServiceDecl n _ _) = pretty n <> text "Service"

join :: Doc -> [Doc] -> Doc
join _ []        = empty
join _ [h]       = h
join d (h:i:t)   = join d $ (h <> d <> i) : t

newtype MethodSignature = MethodSignature { getSignature :: Method }

printMethodSignature :: Method -> Doc
printMethodSignature (Method r n ps _) = pretty r
    <+> (pretty n <> (parens $ join (comma <> space) (map printParam ps)))
    where printParam (Field n ty) = pretty ty <+> pretty n

instance Pretty MethodSignature where
    pretty = printMethodSignature . getSignature

newtype MethodDeclaration = MethodDeclaration { getDeclaration :: Method }

instance Pretty MethodDeclaration where
    pretty = (<> semi) . pretty . MethodSignature . getDeclaration

newtype MethodImplementation = MethodImplementation { getImplementation :: Method }

printServiceImplMethod :: Method -> Doc
printServiceImplMethod m@(Method _ _ _ ss) = vsep
    [ green $ text "@Override"
    , (yellow $ text "public") <+> pretty (MethodSignature m) <+> (white lbrace)
    , indent 2 (vsep $ map pretty ss)
    , white rbrace
    ]

instance Pretty MethodImplementation where
    pretty = printServiceImplMethod . getImplementation

instance Pretty Statement where
    pretty (Assign n (Just ty) expr) = text "final" <+> pretty ty <+> pretty n <+> equals <+> pretty expr <> semi

instance Pretty Expr where
    pretty _ = empty

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
