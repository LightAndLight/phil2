{-# language OverloadedLists #-}
module Phil.Parser
  ( Span(..)
  , parseExpr
  , parseTypeScheme
  , parseDefinition
  , parseDefinitions
  , expr
  , type_
  , typeScheme
  , definition
  , definitions
  )
where

import Control.Applicative
  ((<|>), (<**>), many, some, optional)
import Data.Semigroup ((<>))
import Data.String (IsString)
import Text.Trifecta
  (DeltaParsing, Result(..), Span(..), Spanned(..), spanned, spanning,
   parseString)
import Text.Parser.Combinators ((<?>), Parsing, between, chainr1, try, sepBy1)
import Text.Parser.Char (CharParsing, char, string, letter, digit, upper)
import Text.Parser.Token (IdentifierStyle(..), ident, runUnspaced)
import Text.Parser.Token.Highlight (Highlight(..))

import Phil.Core (Expr(..), Type(..), TypeScheme(..), Definition(..))

parseExpr :: String -> Either String (Expr (Type Span) Span)
parseExpr s =
  case parseString expr mempty s of
    Failure err -> Left $ show err
    Success a -> Right a

parseTypeScheme :: IsString s => String -> Either String (TypeScheme Span s)
parseTypeScheme s =
  case parseString typeScheme mempty s of
    Failure err -> Left $ show err
    Success a -> Right a

parseDefinition :: String -> Either String (Definition Span)
parseDefinition s =
  case parseString definition mempty s of
    Failure err -> Left $ show err
    Success a -> Right a

parseDefinitions :: String -> Either String [Definition Span]
parseDefinitions s =
  case parseString definitions mempty s of
    Failure err -> Left $ show err
    Success a -> Right a

nonNewline :: CharParsing m => m Char
nonNewline = char ' ' <|> char '\t' <?> "space"

idStyle :: CharParsing m => IdentifierStyle m
idStyle =
  IdentifierStyle
  { _styleName = "identifier"
  , _styleStart = letter
  , _styleLetter = letter <|> digit
  , _styleReserved = ["forall"]
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }

ctorStyle :: CharParsing m => IdentifierStyle m
ctorStyle =
  IdentifierStyle
  { _styleName = "constructor"
  , _styleStart = upper
  , _styleLetter = letter <|> digit
  , _styleReserved = []
  , _styleHighlight = Constructor
  , _styleReservedHighlight = ReservedConstructor
  }

identifier :: (DeltaParsing m, IsString s) => m s
identifier = runUnspaced (ident idStyle)

constructor :: (DeltaParsing m, IsString s) => m s
constructor = runUnspaced (ident ctorStyle)

typeScheme :: (DeltaParsing m, IsString s) => m (TypeScheme Span s)
typeScheme =
  (\a (b :~ bAnn) ->
     case a of
       Nothing -> Forall (Just bAnn) [] b
       Just (ann1, a' :~ aAnn, ann2) ->
         Forall (Just $ ann1 <> aAnn <> ann2 <> bAnn) a' b) <$>
  optional
    ((,,) <$>
     spanning (string "forall" *> some nonNewline) <*>
     spanned (sepBy1 identifier (some nonNewline)) <*>
     spanning (many nonNewline <* char '.' <* many nonNewline)) <*>
  spanned type_

type_ :: (DeltaParsing m, IsString s) => m (Type Span s)
type_ = compound
  where
    compound = tyArr

    tyArr =
      (\(val :~ _) -> val) <$>
      chainr1
        (spanned atom)
        ((\ann (l :~ lAnn) (r :~ rAnn) ->
            let
              ann' = lAnn <> ann <> rAnn
            in
              TyArr (Just ann') l r :~ ann') <$>
         spanning (many nonNewline *> string "->" *> many nonNewline))

    atom =
      var <|>
      ctor <|>
      between (char '(' *> many nonNewline) (many nonNewline *> char ')') type_

    ctor = (\(val :~ ann) -> TyCtor (Just ann) val) <$> spanned constructor
    var = (\(val :~ ann) -> TyVar (Just ann) val) <$> spanned identifier

chainl1' :: Parsing m => m a -> m (a -> a -> a) -> m a
chainl1' p op = scan where
  scan = p <**> rst
  rst = try ((\f y g x -> g (f x y)) <$> op <*> p) <*> rst <|> pure id
{-# INLINE chainl1' #-}

expr :: DeltaParsing m => m (Expr (Type Span) Span)
expr = compound
  where
    compound = lambda <|> ann'd

    ann'd =
      (\(x :~ xAnn) y ->
         case y of
           Nothing -> x
           Just (aAnn, y' :~ yAnn) ->
             Ann (Just $ xAnn <> aAnn <> yAnn) x y') <$>
      spanned app <*>
      optional
        ((,) <$>
         spanning (many nonNewline *> char ':' *> many nonNewline) <*>
         spanned type_)

    lambda =
      (\aAnn (b :~ bAnn) cAnn (d :~ dAnn) ->
         Abs (Just $ aAnn <> bAnn <> cAnn <> dAnn) b d) <$>
      spanning (char '\\' *> many nonNewline) <*>
      spanned identifier <*>
      spanning (many nonNewline *> string "->" *> many nonNewline) <*>
      spanned expr

    app =
      (\(val :~ _) -> val) <$>
      chainl1'
        (spanned atom)
        ((\ann (l :~ lAnn) (r :~ rAnn) ->
            let
              ann' = lAnn <> ann <> rAnn
            in
              App (Just $ lAnn <> ann <> rAnn) l r :~ ann') <$>
        spanning (some nonNewline))

    var = (\(val :~ ann) -> Var (Just ann) val) <$> spanned identifier

    hole = (\ann -> Hole (Just ann)) <$> spanning (string "??")

    quote =
      (\(val :~ ann) -> Quote (Just ann) val) <$>
      spanned (char '\'' *> many nonNewline *> atom)

    unquote =
      (\(val :~ ann) -> Unquote (Just ann) val) <$>
      spanned (char '$' *> many nonNewline *> atom)

    int =
      (\(val :~ ann) -> Int (Just ann) val) <$>
      spanned (read <$> some digit)

    atom =
      between (char '(' *> many nonNewline) (many nonNewline *> char ')') expr <|>
      var <|>
      quote <|>
      unquote <|>
      int <|>
      hole

definition :: DeltaParsing m => m (Definition Span)
definition =
  fmap (\(val :~ ann) -> val { _definitionAnn = Just ann}) .
  spanned $
  (\name tOrE ->
     case tOrE of
       Left t -> DefTypeSig Nothing name t
       Right e -> DefValue Nothing name e) <$>
  identifier <*
  many nonNewline <*>
  (Left <$ char ':' <* many nonNewline <*> typeScheme <|>
   Right <$ char '=' <* many nonNewline <*> expr)

definitions :: DeltaParsing m => m [Definition Span]
definitions =
  sepBy1
    definition
    (between
       (many nonNewline)
       (many $ nonNewline <|> char '\n')
       (char ';'))
