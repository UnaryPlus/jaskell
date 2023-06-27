{-|
Module     : Jaskell.Quote
Copyright  : (c) Owen Bechtel, 2023
License    : MIT
Maintainer : ombspring@gmail.com
Stability  : experimental

The 'jsl' quasiquoter converts Jaskell syntax into Haskell syntax.

A Jaskell expression is a sequence of commands. An command is one of the following:

* A Haskell identifier, optionally preceded by @$@, @#@, @?@, @!@, or @&@. 
  The identifier can be qualified or unqualified, and can be lowercase (function) or uppercase (data constructor).
  It cannot be an operator.

    * An identifier @x@ translates to @x@ if @x@ is a function, and @'Jaskell.push' x@ if @x@ is a data constructor.
    * @$x@ translates to @'Jaskell.liftS' x@. For example, @$reverse@ will reverse the list on top of the stack.
    * @#x@ translates to @'Jaskell.liftS2' x@. For example, @#gcd@ will pop the top two values and push their gcd.
    * @?x@ translates to @'Jaskell.pushM' x@. For example, @?getLine@ will execute 'getLine' and push the result.
    * @!x@ translates to @'Jaskell.popM' x@. For example, @!putStrLn@ will print the string on top of the stack.
    * @&x@ translates to @'Jaskell.liftSM' x@. For example, @&readFile@ will pop a file path and push the contents of that file.
  
* A Haskell operator. 
  This translates to 'Jaskell.liftS2' applied to the operator.

* A list of zero or more expressions, surrounded in square brackets and separated by commas.
  For example, @[ 1, 3, 4 1 + ]@ pushes the list @[ 1, 3, 5 ]@ onto the stack.

* A tuple of two expressions. 
  For example, @( 0, "a" "b" ++ )@ pushes the tuple @( 0, "ab" )@ onto the stack.

* An empty tuple. This translates to @'Jaskell.push' ()@.

* An integer, floating-point, character, or string literal.
  For example, @5@ translates to @'Jaskell.push' 5@.

* A (potentially empty) expression surrounded in curly brackets. 
  This pushes the translation of the expression onto the stack, allowing for higher-order programming.

Jaskell programs can be preceded by zero or more definitions, each of the form @DEF name = expr ;@. 
Definitions can be recursive and even mutually recursive.
-}

{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Jaskell.Quote 
  ( -- * Quasiquoter
    jsl
    -- * Parser internals
  , NameMode(..), Name(..), Literal(..), Command(..), Expr(..), Program(..)
  , Parser, parseName, parseLiteral, parseCommand, parseExpr, parseProgram
  ) where

import Data.Void (Void)
import Control.Monad (void)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List.NonEmpty (NonEmpty((:|)))
import Control.Category ((>>>))
import qualified Control.Category as Cat

import qualified Text.Megaparsec as M
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH.Lib

import qualified Jaskell
import qualified Jaskell.Prelude as Pre

-- | Embed a Jaskell program into Haskell.
jsl :: QuasiQuoter
jsl = QuasiQuoter 
  { quoteExp = quote 
  , quotePat = undefined 
  , quoteType = undefined
  , quoteDec = undefined 
  }

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

data Command
  = Name NameMode Name
  | Op String
  | List [Expr]
  | Tup Expr Expr
  | Quote (Maybe Expr)
  | Lit Literal 
  deriving (Eq, Show)

newtype Expr = Expr (NonEmpty Command)
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

parseList :: Parser Command
parseList = List <$ symbol '[' <*> M.sepBy parseExpr (symbol ',') <* symbol ']'

parseTup :: Parser Command
parseTup = do
  M.notFollowedBy (M.chunk "()")
  Tup <$ symbol '(' <*> parseExpr <* symbol ',' <*> parseExpr <* symbol ')'

parseCommand :: Parser Command
parseCommand = M.choice
  [ Name <$> parseNameMode <*> parseName <* spaces
  , Op <$> parseOp <* spaces
  , parseList 
  , parseTup 
  , Quote <$ symbol '{' <*> M.optional parseExpr <* symbol '}'
  , Lit <$> parseLiteral <* spaces
  ]

parseExpr :: Parser Expr
parseExpr = do
  cmds <- (:|) <$> parseCommand <*> M.many parseCommand
  return (Expr cmds)

parseDef :: Parser (String, Expr)
parseDef = (,) <$ M.chunk "DEF" <* spaces <*> lowerName <* spaces <* symbol '=' <*> parseExpr <* symbol ';'

parseProgram :: Parser Program
parseProgram = Program <$> M.many parseDef <*> parseExpr

initialState :: s -> M.SourcePos -> M.State s Void 
initialState input pos = 
  M.State
    { M.stateInput = input
    , M.stateOffset = 0
    , M.statePosState = 
        M.PosState
          { M.pstateInput = input
          , M.pstateOffset = 0
          , M.pstateSourcePos = pos
          , M.pstateTabWidth = M.defaultTabWidth
          , M.pstateLinePrefix = ""
          }
      , M.stateParseErrors = []
    }

quote :: String -> ExpQ
quote input = do
  loc <- TH.location
  let file = TH.loc_filename loc
      (line, col) = TH.loc_start loc
      state = initialState input (M.SourcePos file (M.mkPos line) (M.mkPos col)) 
      parse = spaces >> (parseProgram <* M.eof)

  case snd (M.runParser' parse state) of 
    Left errors -> fail (M.errorBundlePretty errors)
    Right prog -> convertProgram prog

comp :: Foldable t => t ExpQ -> ExpQ
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

convertCommand :: Command -> ExpQ
convertCommand = \case
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
  
  Quote x -> appE (varE 'Jaskell.push) (maybe (varE 'Cat.id) convertExpr x)
  
  Lit lit -> appE (varE 'Jaskell.push) (convertLiteral lit)

convertExpr :: Expr -> ExpQ
convertExpr (Expr xs) = comp (fmap convertCommand xs)

convertDef :: (String, Expr) -> DecQ
convertDef (n, x) = funD (TH.mkName n) [ clause [] (normalB (convertExpr x)) [] ]

convertProgram :: Program -> ExpQ
convertProgram (Program defs x) = 
  if null defs 
    then convertExpr x
    else letE (map convertDef defs) (convertExpr x)

