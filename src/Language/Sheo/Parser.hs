module Language.Sheo.Parser where

import Control.Applicative
import Language.Sheo.Types
import Text.Parser.Token.Style
import Text.Trifecta
import Text.Trifecta.Delta

-- helpers to remove
parse :: Parser a -> String -> Result a
parse p s = parseString p (Columns 0 0) s

fromSuccess :: Result a -> a
fromSuccess (Success a) = a

program :: Parser Program
program = Program <$> many decl

decl :: Parser Decl
decl = dataDecl <|> serviceDecl

dataDecl :: Parser Decl
dataDecl = DataDecl <$> (term "data" () *> name) <*> braces (sepBy field semi)
    where fields = sepBy field newline

serviceDecl :: Parser Decl
serviceDecl = ServiceDecl
    <$> (term "service" () *> name)
    <*> parens (commaSep dep)
    <*> braces (many method)

dep :: Parser (Name, Maybe Ty)
dep = (,) <$> name <*> (Just <$> ty)

method :: Parser Method
method = go <$> (term "method" () *> name) <*> parens argList <*> (colon *> ty) <*> braces stmts
    where go name args ret body = Method ret name args body

argList :: Parser [Field]
argList = commaSep field

field :: Parser Field
field = Field <$> name <*> (colon *> ty)

stmts :: Parser [Statement]
stmts = commaSep stmt

stmt :: Parser Statement
stmt = try (Assign <$> (term "let" () *> name) <*> (colon *> (Just <$> ty)) <*> (equals *> expr))
   <|> (Simply <$> expr)

equals :: Parser Char
equals = token $ char '='

ws :: Parser ()
ws = pure () <* many (oneOf " \r\t\n")

expr :: Parser Expr
expr = I . fromInteger <$> natural

ty :: Parser Ty
ty = chainr1 tyBase (term "->" TFun)

tyBase :: Parser Ty
tyBase = term "Int" TInt
     <|> term "String" TString

name :: Parser Name
name = Name <$> ident haskellIdents

term :: String -> a -> Parser a
term s x = pure x <* (token $ string s)