module Language.Sheo.Types where

import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S

newtype Program = Program { programDecls :: [Decl] }
    deriving (Show)

data Decl
    = DataDecl [Field]
    | ServiceDecl Name [Name] [Method]
    deriving (Show)

newtype Method = Method { methodStatements :: [Statement] }
    deriving (Show)

data Statement
    = Assign Name Expr
    | Simply Expr
    deriving (Show)

data Expr
    = Var Int
    | B Bool
    | I Int
    | S String
    | List [Expr]
    | Map (M.HashMap Expr Expr)
    | Set (S.HashSet Expr)
    | Lam Expr
    | App Expr Expr
    | FMap Expr Expr
    | Fold Expr Expr Expr

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
    deriving (Show)

newtype Name = Name { getName :: String }
    deriving (Show)