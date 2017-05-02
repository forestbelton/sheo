module Language.Sheo.Rewrite.Rewrite (Rewrite(..), runRewrite) where

import Data.Monoid
import Language.Sheo.Types

newtype Rewrite = Rewrite { getRewrite :: Expr -> Expr }

runRewrite :: Rewrite -> Program -> Program
runRewrite r (Program decls) = Program decls'
    where decls' = map (mapDecl r) decls

-- TODO: lenses...lol
mapDecl :: Rewrite -> Decl -> Decl
mapDecl r d@(DataDecl _ _) = d
mapDecl r (ServiceDecl n deps ms) = ServiceDecl n deps $ map (mapMethod r) ms

mapMethod :: Rewrite -> Method -> Method
mapMethod r (Method r1 n ps stmts) = Method r1 n ps $ map (mapStmt r) stmts

mapStmt :: Rewrite -> Statement -> Statement
mapStmt (Rewrite r) (Assign n ty expr) = Assign n ty (r expr)
mapStmt (Rewrite r) (Simply expr) = Simply (r expr)

instance Monoid Rewrite where
    mempty = Rewrite id
    mappend (Rewrite a) (Rewrite b) = Rewrite (a . b)
