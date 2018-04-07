{-# language OverloadedLists #-}
module Phil.Parser where

import Control.Applicative
  ((<|>), (<**>), liftA2, many, some, optional)
import Control.Lens.Getter ((^.))
import Data.Semigroup ((<>))
import Data.String (IsString)
import Text.Trifecta (DeltaParsing, Span, Spanned(..), spanned, spanning)
import Text.Parser.Combinators (Parsing, between, chainr1, try)
import Text.Parser.Char (CharParsing, char, string, letter, digit, space, upper)
import Text.Parser.Token (IdentifierStyle(..), ident, runUnspaced)
import Text.Parser.Token.Highlight (Highlight(..))

import Phil.Core (Expr(..), Type(..), exprAnn, typeAnn)

idStyle :: CharParsing m => IdentifierStyle m
idStyle =
  IdentifierStyle
  { _styleName = "identifier"
  , _styleStart = letter
  , _styleLetter = letter <|> digit
  , _styleReserved = []
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
         spanning (many space *> string "->" *> many space))

    atom = var <|> ctor
    ctor = (\(val :~ ann) -> TyCtor (Just ann) val) <$> spanned constructor
    var = (\(val :~ ann) -> TyVar (Just ann) val) <$> spanned identifier

chainl1' :: Parsing m => m a -> m (a -> a -> a) -> m a
chainl1' p op = scan where
  scan = p <**> rst
  rst = try ((\f y g x -> g (f x y)) <$> op <*> p) <*> rst <|> pure id
{-# INLINE chainl1' #-}

expr :: DeltaParsing m => m (Expr Span)
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
         spanning (many space *> char ':' *> many space) <*>
         spanned type_)

    lambda =
      (\aAnn (b :~ bAnn) cAnn (d :~ dAnn) ->
         Abs (Just $ aAnn <> bAnn <> cAnn <> dAnn) b d) <$>
      spanning (char '\\' *> many space) <*>
      spanned identifier <*>
      spanning (many space *> string "->" *> many space) <*>
      spanned expr

    app =
      chainl1'
        atom
        ((\ann l r ->
            App
              (liftA2 (<>) ((<> ann) <$> (l ^. exprAnn)) (r ^. exprAnn))
              l
              r) <$>
         spanning (some space))

    var = (\(val :~ ann) -> Var (Just ann) val) <$> spanned identifier

    atom =
      between (char '(' *> many space) (many space *> char ')') expr <|>
      var
