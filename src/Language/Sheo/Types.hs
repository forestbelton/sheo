module Language.Sheo.Types where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

newtype Program = Program { programDecls :: [Decl] }
    deriving (Show)

data Decl
    = DataDecl Name [Field]
    | ServiceDecl Name [(Name, Maybe Ty)] [Method]
    deriving (Show)

data Method = Method
    { methodReturnType :: Ty
    , methodName :: Name
    , methodParams :: [Field]
    , methodStatements :: [Statement]
    }
    deriving (Show)

data Statement
    = Assign Name (Maybe Ty) Expr
    | Simply Expr
    deriving (Show)

data Expr
    = Var Name (Maybe Ty) Int
    | B Bool
    | I Int
    | S String
    | List [Expr]
    | Map (M.HashMap Expr Expr)
    | Set (S.HashSet Expr)
    | Lam Name Expr
    | App Expr Expr
    | FMap Expr Expr
    | Fold Expr Expr Expr
    deriving (Show)

data Field = Field Name Ty
    deriving (Show)

data Ty
    = TBool
    | TInt
    | TString
    | TFun Ty Ty
    | TList Ty
    | TMap Ty Ty
    | TSet Ty
    | TClass String
    deriving (Show)

newtype Name = Name { getName :: String }
    deriving (Show)