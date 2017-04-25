module Language.Sheo.Printer where

import Language.Sheo.Types
import Text.PrettyPrint.ANSI.Leijen

class Printable a where
    toDoc :: a -> Doc

printDataDecl :: Decl -> Doc
printDataDecl (DataDecl n fs) = vsep
    [ text "@Value.Immutable"
    , text "public" <+> text "interface" <+> toDoc n <+> lbrace
    , empty
    , indent 2 (vsep $ fs >>= \x -> [toDoc x, empty])
    , rbrace
    ]

printServiceInterface :: Decl -> Doc
printServiceInterface (ServiceDecl n _ ms) = vsep
    [ text "public" <+> text "interface" <+> toDoc n <> text "Service" <+> lbrace
    , empty
    , indent 2 (vsep $ map printServiceMethod ms)
    , rbrace
    ]

printServiceMethod :: Method -> Doc
printServiceMethod (Method r n ps _) = sep
    [ toDoc r
    , toDoc n <> (parens $ sep $ punctuate (comma <> space) (map printParam ps)) <> semi
    ]
    where printParam (Field n ty) = toDoc ty <+> toDoc n

commaSep :: [Doc] -> Doc
commaSep []      = empty
commaSep [h]     = h
commaSep (h:i:t) = commaSep $ (h <> comma <+> i) : t

instance Printable Field where
    toDoc (Field n ty) = toDoc ty <+> toDoc n <> parens empty <> semi

instance Printable Ty where
    toDoc ty = case ty of
                TBool    -> text "Boolean"
                TInt     -> text "Integer"
                TString  -> text "String"
                TFun a b -> text "Function" <> langle <> toDoc a <> comma <> toDoc b <> rangle
                TList a  -> text "List" <> lparen <> toDoc a <> rparen
                TSet a   -> text "Set" <> lparen <> toDoc a <> rparen

instance Printable Name where
    toDoc = text . getName
