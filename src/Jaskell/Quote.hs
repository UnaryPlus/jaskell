{-# LANGUAGE LambdaCase #-}
module Jaskell.Quote (jsl) where

import Data.Void (Void)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)

import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Lib (ExpQ)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

import qualified Jaskell
import qualified Jaskell.Prelude as Pre

jsl :: QuasiQuoter
jsl = QuasiQuoter { quoteExp = quote }

-- must be followed by function/constructor name
-- $    map function over top value
-- #    map binary function over top two values
-- ?    push result of monadic action
-- !    pop value and execute monadic action
-- &    map monadic action over top value

-- operator        map operator over top two values
-- constructor     push constructor onto stack  
-- function        apply function to stack   
-- literal         (num, string, char, unit) push value onto stack
-- [ e1, ... en ]  syntactic sugar for e1 ... en nil cons ... cons
-- ( a, b )        syntactic sugar for a b pair
-- { expr }        push translation of expr onto stack

data NameMode
  = Bare
  | LiftS  
  | LiftS2 
  | PushM  
  | PopM  
  | LiftSM 

data Name
  = Fun [String] String
  | Ctor [String] String

data Literal
  = Char Char
  | String String
  | Integer Integer
  | Rational Rational
  | Unit

data Atom
  = Name NameMode Name
  | Op String
  | List [Expr]
  | Tup Expr Expr
  | Quote Expr
  | Lit Literal 

newtype Expr = Expr [Atom]

data Program = Program [(String, Expr)] Expr

type Parser = M.Parsec Void String

parseNameMode :: Parser NameMode
parseNameMode = M.choice
  [ LiftS <$ M.single '$'
  , LiftS2 <$ M.single '#'
  , PushM <$ M.single '?'
  , PopM <$ M.single '!'
  , LiftSM <$ M.single '&'
  , return Bare
  ]

isNameChar :: Char -> Bool
isNameChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` "'_"

lowerName :: Parser String
lowerName = (:) <$> M.satisfy isAsciiLower <*> M.takeWhileP isNameChar

upperName :: Parser String
upperName = do
  M.notFollowedBy (M.chunk "DEF" >> M.notFollowedBy (M.satisfy isNameChar))
  (:) <$> M.satisfy isAsciiUpper <*> M.takeWhileP isNameChar

parseName :: Parser Name
parseName = reverseModules <$> parseName' []
  where
    parseName' modules = M.choice
      [ Fun modules <* lowerName
      , do m <- upperName
           M.choice 
             [ M.single '.' >> parseName' (m : modules)
             , return (Ctor modules m)
             ]
      ]
    
    reverseModules = \case
      Fun ms n -> Fun (reverse ms) n
      Ctor ms n -> Ctor (reverse ms) n

isOpChar :: Char -> Bool
isOpChar c = c `elem` "!#$%&*+./<=>?@\^|-~:"

parseOp :: Parser String
parseOp = do
  M.notFollowedBy $ M.choice
    [ M.single '-' >> M.satisfy isDigit
    , M.single ';' >> M.notFollowedBy (M.satisfy isOpChar)
    , M.single '=' >> M.notFollowedBy (M.satisfy isOpChar)
    ]
  M.takeWhile1P isOpChar

parseLiteral :: Parser Literal
parseLiteral = M.choice
  [ Char <$ M.single '\'' <*> L.charLiteral <$> M.single '\''
  , String <$ M.single '"' <*> M.manyTill L.charLiteral (M.single '"')
  , Integer <$> L.decimal
  , Rational <$> L.float
  , Unit <$ M.chunk "()"
  ]

spaces :: Parser ()
spaces = L.space C.space1
  (L.skipLineComment "--")
  (L.skipBlockCommentNested "{-" "-}")

symbol :: Char -> Parser ()
symbol c = M.single c >> spaces

parseList :: Parser Atom
parseList = List <$ symbol '[' <*> M.sepBy parseExpr (symbol ',') <* symbol ']'

parseTup :: Parser Atom
parseTup = do
  M.notFollowedBy (M.chunk "()")
  Tup <$ symbol '(' <*> parseExpr <* symbol ',' <*> parseExpr <* symbol ')'

parseAtom :: Parser Atom
parseAtom = M.choice
  [ Name <$> parseNameMode <*> parseName <* spaces
  , Op <$> parseOp <* spaces
  , parseList 
  , parseTup 
  , Quote <$ symbol '{' <*> parseExpr <* symbol '}'
  , Lit <$> parseLiteral <* spaces
  ]

parseExpr :: Parser Expr
parseExpr = Expr <$> M.many1 parseAtom

parseDef :: Parser (String, Expr)
parseDef = (,) <$ M.chunk "DEF" <* spaces <*> lowerName <* spaces <* symbol '=' <*> parseExpr <* symbol ';'

parseProgram :: Parser Program
parseProgram = Program <$> M.many parseDef <*> parseExpr

setLoc :: (Int, Int) -> Parser ()
setLoc (line, col) =
  updateParserState $ \state ->
    let posState = statePosState state
        sourcePos = pstateSourcePos posState
        sourcePos' = sourcePos { sourceLine = line, sourceColumn = col }
        posState' = posState { pstateSourcePos = sourcePos' }
    in state { statePosState = posState' }

quote :: String -> ExpQ
quote input = do
  loc <- TH.location
  let file = TH.loc_filename loc
      (line, col) = TH.start_loc loc
      parse = setLoc (line, col) >> spaces >> (parseProgram <* M.eof)
  case M.runParser parse file input of 
    Left errors -> fail (M.errorBundlePretty errors)
    Right prog -> convertProgram prog

comp :: [ExpQ] -> ExpQ
comp = foldr1 (\f g -> infixE f (var '(.)) g)

convertName :: Name -> ExpQ
convertName = \case
  Fun ms n -> varE (TH.mkName (concatMap (++ ".") ms ++ n))
  Ctor ms n -> conE (TH.mkName (concatMap (++ ".") ms ++ n))

convertLiteral :: Literal -> ExpQ
convertLiteral = \case
  Char c -> litE (charL c)
  String s -> litE (stringL s)
  Integer i -> litE (integerL i)
  Rational r -> litE (rationalL r)
  Unit -> tupE []

convertAtom :: Atom -> ExpQ
convertAtom = \case
  Name mode n -> 
    let nexp = convertName n in
    case mode of
      Bare -> case n of
        Fun _ _ -> nexp
        Ctor _ _ -> appE 'Jaskell.push nexp
      LiftS -> appE 'Jaskell.liftS nexp
      LiftS2 -> appE 'Jaskell.liftS2 nexp
      PushM -> appE 'Jaskell.pushM nexp
      PopM -> appE 'Jaskell.popM nexp
      LiftSM -> appE 'Jaskell.liftSM nexp
  
  Op op -> appE 'Jaskell.liftS2 (infixE Nothing (varE (TH.mkName op)) Nothing)
  
  List xs -> comp $ 
    map convertExpr xs
    ++ [ appE 'Jaskell.push (listE []) ]
    ++ replicate (length xs) 'Pre.cons
  
  Tup x1 x2 -> comp [ convertExpr x1, convertExpr x2, 'Pre.pair ]
  
  Quote x -> appE 'Jaskell.push (convertExpr x)
  
  Lit lit -> convertLiteral lit

convertExpr :: Expr -> ExpQ
convertExpr (Expr xs) -> comp (map convertAtom xs)

convertDef :: (String, Expr) -> DecQ
convertDef (n, x) = funD (newName n) [ clause [] (normalB x) [] ]

convertProgram :: Program -> ExpQ
convertProgram (Program defs x) = letE (map convertDef defs) (convertExpr x)

