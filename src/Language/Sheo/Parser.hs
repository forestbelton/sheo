module Language.Sheo.Parser (parse) where

import qualified Data.HashSet as S

import Control.Applicative
import Language.Sheo.Types
import Text.Parser.Token.Style
import Text.Trifecta
import Text.Trifecta.Delta

parse :: String -> IO (Maybe Program)
parse = parseFromFile program

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
stmts = many (stmt <* semi)

stmt :: Parser Statement
stmt = try (Assign <$> (term "let" () *> name) <*> (colon *> (Just <$> ty)) <*> (equals *> expr))
   <|> (Simply <$> expr)

equals :: Parser Char
equals = token $ char '='

ws :: Parser ()
ws = pure () <* many (oneOf " \r\t\n")

expr :: Parser Expr
expr = chainl1 baseExpr op

op :: Parser (Expr -> Expr -> Expr)
op = term "+" (BOp Add)

baseExpr :: Parser Expr
baseExpr = int
   <|> bool
   <|> str
   <|> list
   <|> lam
   <|> var
   <|> parens expr

int :: Parser Expr
int = (I . fromInteger <$> natural)

bool :: Parser Expr
bool = B <$> (term "true" True <|> term "false" False)

str :: Parser Expr
str = S <$> stringLiteral

list :: Parser Expr
list = List <$> (brackets $ commaSep expr)

lam :: Parser Expr
lam = Lam <$> (term "\\" () *> name) <*> (term "->" () *> expr)

var :: Parser Expr
var = Var <$> name <*> pure Nothing

ty :: Parser Ty
ty = chainr1 tyBase (term "->" TFun)

tyBase :: Parser Ty
tyBase = term "Int" TInt
     <|> term "String" TString

name :: Parser Name
name = Name <$> ident haskellIdents

term :: String -> a -> Parser a
term s x = pure x <* (token $ string s)