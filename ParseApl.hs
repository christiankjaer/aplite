module ParseApl where
import System.IO
import Control.Monad
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import qualified Text.ParserCombinators.Parsec.Token as Token

data Expr = Assign Target Expr
          | MonApp MonId Expr
          | SubE SubExpr
          | DyaApp SubExpr DyaId Expr
            deriving (Show)

data SubExpr = SimE SimExpr
             | ArrE SimExpr [Expr]
               deriving (Show)

data SimExpr = AId ArrId
             | EExp Expr
               deriving (Show)

data Target = Var String
            | VarArr String [Expr]
              deriving (Show)

data MonId = Roll
           | Not
           | Abs
           | Iota
           | Neg
             deriving (Show)

data DyaId = Add
           | Subtract
           | Multiply
           | Divide
           | Exp
           | Deal
           | Take
           | Drop
           | Cat
             deriving (Show)

data ArrId = Const Value
           | VarId String
             deriving (Show)

data Value = Num [Integer]
           | Text String
             deriving (Show)

languageDef =
    emptyDef { Token.commentLine = "//"
             , Token.identStart = letter
             , Token.identLetter = alphaNum
             , Token.reservedOpNames = ["<-", "+", "-", "*", "%", "~"
                                       , "/", "\\", ",", "#", "?", "^"
                                       , "o.", "i.", "take", "drop", "iota"]
             }

lexer = Token.makeTokenParser languageDef

identifier = Token.identifier lexer
reservedOp = Token.reservedOp lexer
parens     = Token.parens     lexer
brackets   = Token.brackets   lexer
natural    = Token.natural    lexer
semiSep    = Token.semiSep    lexer
whiteSpace = Token.whiteSpace lexer
stringLit  = Token.stringLiteral lexer


aplParser :: Parser Expr
aplParser = whiteSpace >> expr

expr :: Parser Expr
expr =  try assign
    <|> try dyaApp
    <|> try monApp
    <|> subE

assign :: Parser Expr
assign =
    do name <- target
       reservedOp "<-"
       e <- expr
       return $ Assign name e

monApp :: Parser Expr
monApp =
    do mid <- monId
       e <- expr
       return $ MonApp mid e

subE :: Parser Expr
subE =
    do se <- subExpr
       return $ SubE se

dyaApp :: Parser Expr
dyaApp =
    do se <- subExpr
       did <- dyaId
       e <- expr
       return $ DyaApp se did e

subExpr :: Parser SubExpr
subExpr =  try arrE
       <|> (simExpr >>= \se -> return $ SimE se)

arrE :: Parser SubExpr
arrE =
    do se <- simExpr
       e <- brackets (semiSep expr)
       return $ ArrE se e

simExpr :: Parser SimExpr
simExpr =  (arrId >>= \aid -> return $ AId aid)
       <|> (parens expr >>= \e -> return $ EExp e)

target :: Parser Target
target =  try varArr
      <|> (identifier >>= \id -> return $ Var id)

varArr :: Parser Target
varArr =
    do id <- identifier
       e <- brackets (semiSep expr)
       return $ VarArr id e

arrId :: Parser ArrId
arrId =  (val >>= \v -> return $ Const v)
     <|> (identifier >>= \id -> return $ VarId id)

val :: Parser Value
val =  (many1 natural >>= \x -> return $ Num x)
   <|> (stringLit >>= \lit -> return $ Text lit)


monId :: Parser MonId
monId =  (reservedOp "?" >> return Roll)
     <|> (reservedOp "~" >> return Not)
     <|> (reservedOp "|" >> return Abs)
     <|> (reservedOp "iota" >> return Iota)
     <|> (reservedOp "-" >> return Neg)

dyaId :: Parser DyaId
dyaId =  (reservedOp "+" >> return Add)
     <|> (reservedOp "-" >> return Subtract)
     <|> (reservedOp "*" >> return Multiply)
     <|> (reservedOp "%" >> return Divide)
     <|> (reservedOp "^" >> return Exp)
     <|> (reservedOp "?" >> return Deal)
     <|> (reservedOp "take" >> return Drop)
     <|> (reservedOp "drop" >> return Take)
     <|> (reservedOp "," >> return Cat)

parseString :: String -> Expr
parseString str =
    case parse aplParser "" str of
        Left e -> error $ show e
        Right r -> r
