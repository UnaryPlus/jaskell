{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Jaskell.Quote 
  ( jsl
  , NameMode(..), Name(..), Literal(..), Atom(..), Expr(..), Program(..)
  , Parser, parseName, parseLiteral, parseAtom, parseExpr, parseProgram
  ) where

import Data.Void (Void)
import Control.Monad (void)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Control.Category ((>>>))

import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Lib

import qualified Jaskell
import qualified Jaskell.Prelude as Pre

jsl :: QuasiQuoter
jsl = QuasiQuoter 
  { quoteExp = quote 
  , quotePat = undefined 
  , quoteType = undefined
  , quoteDec = undefined 
  }

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
  deriving (Eq, Show)

data Name
  = Fun [String] String
  | Ctor [String] String
  deriving (Eq, Show)

data Literal
  = Char Char
  | String String
  | Integer Integer
  | Double Double
  | Unit
  deriving (Eq, Show)

data Atom
  = Name NameMode Name
  | Op String
  | List [Expr]
  | Tup Expr Expr
  | Quote Expr
  | Lit Literal 
  deriving (Eq, Show)

newtype Expr = Expr [Atom]
  deriving (Eq, Show)

data Program = Program [(String, Expr)] Expr
  deriving (Eq, Show)

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
lowerName = (:) <$> M.satisfy isAsciiLower <*> M.takeWhileP (Just "identifier charachter") isNameChar

upperName :: Parser String
upperName = do
  M.notFollowedBy (M.chunk "DEF" >> M.notFollowedBy (M.satisfy isNameChar))
  (:) <$> M.satisfy isAsciiUpper <*> M.takeWhileP (Just "identifier charachter") isNameChar

parseName :: Parser Name
parseName = reverseModules <$> parseName' []
  where
    parseName' modules = M.choice
      [ Fun modules <$> lowerName
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
isOpChar c = c `elem` "!#$%&*+./<=>?@\\^|-~:"

parseOp :: Parser String
parseOp = do
  M.notFollowedBy $ M.choice
    [ M.single '-' >> void (M.satisfy isDigit)
    , M.single '=' >> M.notFollowedBy (M.satisfy isOpChar)
    ]
  M.takeWhile1P (Just "operator character") isOpChar

parseLiteral :: Parser Literal
parseLiteral = M.choice
  [ Char <$ M.single '\'' <*> L.charLiteral <* M.single '\''
  , String <$ M.single '"' <*> M.manyTill L.charLiteral (M.single '"')
  , M.try (Double <$> L.float)
  , Integer <$> L.decimal
  , negative
  , Unit <$ M.chunk "()"
  ]
  where
    negative = do
      _ <- M.single '-'
      M.choice
        [ M.try (Double . negate <$> L.float)
        , Integer . negate <$> L.decimal
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
parseExpr = Expr <$> M.some parseAtom

parseDef :: Parser (String, Expr)
parseDef = (,) <$ M.chunk "DEF" <* spaces <*> lowerName <* spaces <* symbol '=' <*> parseExpr <* symbol ';'

parseProgram :: Parser Program
parseProgram = Program <$> M.many parseDef <*> parseExpr

setLoc :: (Int, Int) -> Parser ()
setLoc (line, col) =
  M.updateParserState $ \state ->
    let posState = M.statePosState state
        sourcePos = M.pstateSourcePos posState
        sourcePos' = sourcePos { M.sourceLine = M.mkPos line, M.sourceColumn = M.mkPos col }
        posState' = posState { M.pstateSourcePos = sourcePos' }
    in state { M.statePosState = posState' }

quote :: String -> ExpQ
quote input = do
  loc <- TH.location
  let file = TH.loc_filename loc
      (line, col) = TH.loc_start loc
      parse = setLoc (line, col) >> spaces >> (parseProgram <* M.eof)
  case M.runParser parse file input of 
    Left errors -> fail (M.errorBundlePretty errors)
    Right prog -> convertProgram prog

comp :: [ExpQ] -> ExpQ
comp = foldr1 (\f g -> infixE (Just f) (varE '(>>>)) (Just g))

convertName :: Name -> ExpQ
convertName = \case
  Fun ms n -> varE (TH.mkName (concatMap (++ ".") ms ++ n))
  Ctor ms n -> conE (TH.mkName (concatMap (++ ".") ms ++ n))

convertLiteral :: Literal -> ExpQ
convertLiteral = \case
  Char c -> litE (charL c)
  String s -> litE (stringL s)
  Integer i -> litE (integerL i)
  Double r -> litE (rationalL (toRational r))
  Unit -> tupE []

convertAtom :: Atom -> ExpQ
convertAtom = \case
  Name mode n -> 
    let nexp = convertName n in
    case mode of
      Bare -> case n of
        Fun _ _ -> nexp
        Ctor _ _ -> appE (varE 'Jaskell.push) nexp
      LiftS -> appE (varE 'Jaskell.liftS) nexp
      LiftS2 -> appE (varE 'Jaskell.liftS2) nexp
      PushM -> appE (varE 'Jaskell.pushM) nexp
      PopM -> appE (varE 'Jaskell.popM) nexp
      LiftSM -> appE (varE 'Jaskell.liftSM) nexp
  
  Op op -> appE (varE 'Jaskell.liftS2) (infixE Nothing (varE (TH.mkName op)) Nothing)
  
  List xs -> comp $ 
    map convertExpr xs
    ++ [ appE (varE 'Jaskell.push) (listE []) ]
    ++ replicate (length xs) (varE 'Pre.cons)
  
  Tup x1 x2 -> comp [ convertExpr x1, convertExpr x2, varE 'Pre.pair ]
  
  Quote x -> appE (varE 'Jaskell.push) (convertExpr x)
  
  Lit lit -> appE (varE 'Jaskell.push) (convertLiteral lit)

convertExpr :: Expr -> ExpQ
convertExpr (Expr xs) = comp (map convertAtom xs)

convertDef :: (String, Expr) -> DecQ
convertDef (n, x) = funD (TH.mkName n) [ clause [] (normalB (convertExpr x)) [] ]

convertProgram :: Program -> ExpQ
convertProgram (Program defs x) = letE (map convertDef defs) (convertExpr x)

