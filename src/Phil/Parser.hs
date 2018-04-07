{-# language OverloadedLists #-}
module Phil.Parser where

import Control.Applicative
  ((<|>), (<**>), liftA2, many, some, optional)
import Control.Lens.Getter ((^.))
import Data.Semigroup ((<>))
import Data.String (IsString)
import Text.Trifecta (DeltaParsing, Span, Spanned(..), spanned, spanning)
import Text.Parser.Combinators ((<?>), Parsing, between, chainr1, try, sepBy1)
import Text.Parser.Char (CharParsing, char, string, letter, digit, upper)
import Text.Parser.Token (IdentifierStyle(..), ident, runUnspaced)
import Text.Parser.Token.Highlight (Highlight(..))

import Phil.Core (Expr(..), Type(..), TypeScheme(..), exprAnn, typeAnn)

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
      chainr1
        atom
        ((\ann l r ->
            TyArr
              (liftA2 (<>) ((<> ann) <$> (l ^. typeAnn)) (r ^. typeAnn))
              l
              r) <$>
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
      chainl1'
        atom
        ((\ann l r ->
            App
              (liftA2 (<>) ((<> ann) <$> (l ^. exprAnn)) (r ^. exprAnn))
              l
              r) <$>
         spanning (some nonNewline))

    var = (\(val :~ ann) -> Var (Just ann) val) <$> spanned identifier

    hole = (\ann -> Hole (Just ann)) <$> spanning (string "??")

    atom =
      between (char '(' *> many nonNewline) (many nonNewline *> char ')') expr <|>
      var <|>
      hole
