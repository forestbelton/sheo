module Language.Sheo.Rewrite.EnStream (enstream) where

import Language.Sheo.Types
import Language.Sheo.Rewrite.Rewrite

enstream :: Rewrite
enstream = Rewrite enstream'

enstream' :: Expr -> Expr
enstream' (Lam n e)      = Lam n $ enstream' e
enstream' e@(FMap _ _)   = Streamed e
enstream' e@(Fold _ _ _) = Streamed e