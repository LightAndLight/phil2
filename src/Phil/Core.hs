{-# language DeriveFunctor, DeriveFoldable, DeriveGeneric, DeriveTraversable #-}
{-# language MultiParamTypeClasses, FlexibleInstances #-}
{-# language TemplateHaskell #-}
{-# language LambdaCase #-}
{-# language StandaloneDeriving, UndecidableInstances #-}
{-# language PatternSynonyms #-}
module Phil.Core where

import Control.Lens.Fold ((^?))
import Control.Lens.Lens (Lens', lens)
import Control.Lens.Plated (Plated(..), gplate, transform)
import Control.Lens.Plated1 (Plated1(..))
import Control.Lens.Prism (prism')
import Control.Lens.TH (makePrisms)
import Control.Lens.Traversal (Traversal)
import Control.Monad (ap)
import Control.Monad.Unify (AsVar(..), HasAnnotation(..), Unifiable(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Char8
import Data.Deriving (deriveOrd1, deriveEq1, deriveShow1)
import GHC.Generics (Generic)
import Data.Int (Int64)
import Text.Trifecta (Span(..))
import Text.Trifecta.Delta (Delta(..))

data Type ann a
  = TyVar (Maybe ann) a
  | TyArr (Maybe ann) (Type ann a) (Type ann a)
  | TyCtor (Maybe ann) String
  deriving (Eq, Show, Functor, Foldable, Traversable, Generic)

typeAnn :: Lens' (Type ann a) (Maybe ann)
typeAnn =
  lens
    (\case
        TyVar ann _ -> ann
        TyArr ann _ _ -> ann
        TyCtor ann _ -> ann)
    (\e ann ->
       case e of
        TyVar _ a -> TyVar ann a
        TyArr _ a b -> TyArr ann a b
        TyCtor _ a -> TyCtor ann a)

data TypeScheme ann a = Forall (Maybe ann) [a] (Type ann a)
  deriving (Eq, Show, Functor, Foldable, Traversable)

deriveEq1 ''Type
deriveOrd1 ''Type
deriveShow1 ''Type

instance Plated1 (Type ann) where; plate1 = gplate

makePrisms ''Type

instance AsVar (Type ann) where
  _Var =
    prism'
      (TyVar Nothing)
      (\case
          TyVar _ a -> Just a
          _ -> Nothing)

instance HasAnnotation (Type ann) (Maybe ann) where
  annotation = typeAnn

instance Monad (Type ann) where
  return = TyVar Nothing
  TyVar _ a >>= f = f a
  TyArr ann a b >>= f = TyArr ann (a >>= f) (b >>= f)
  TyCtor ann a >>= _ = TyCtor ann a

instance Applicative (Type ann) where; pure = return; (<*>) = ap

instance Unifiable (Type ann) where
  toplevelEqual TyArr{} TyArr{} = True
  toplevelEqual (TyCtor _ a) (TyCtor _ b) = a == b
  toplevelEqual _ _ = False

data Expr ty ann
  = Var (Maybe ann) String
  | Abs (Maybe ann) String (Expr ty ann)
  | App (Maybe ann) (Expr ty ann) (Expr ty ann)
  | Hole (Maybe ann)
  | Quote (Maybe ann) (Expr ty ann)
  | String (Maybe ann) String
  | Unquote (Maybe ann) (Expr ty ann)
  | Ann (Maybe ann) (Expr ty ann) (ty String)
  | Int (Maybe ann) Int64
  deriving (Generic)
deriving instance (Eq (ty String), Eq ann) => Eq (Expr ty ann)
deriving instance (Show (ty String), Show ann) => Show (Expr ty ann)

pattern Ctor :: String -> Expr ty ann
pattern Ctor a = Var Nothing a

pattern Ctor1 :: String -> Expr ty ann -> Expr ty ann
pattern Ctor1 a b = App Nothing (Ctor a) b

pattern Ctor2 :: String -> Expr ty ann -> Expr ty ann -> Expr ty ann
pattern Ctor2 a b c = App Nothing (Ctor1 a b) c

pattern Ctor3 :: String -> Expr ty ann -> Expr ty ann -> Expr ty ann -> Expr ty ann
pattern Ctor3 a b c d = App Nothing (Ctor2 a b c) d

pattern Ctor4 :: String -> Expr ty ann -> Expr ty ann -> Expr ty ann -> Expr ty ann -> Expr ty ann
pattern Ctor4 a b c d e = App Nothing (Ctor3 a b c d) e

pattern Ctor5 :: String -> Expr ty ann -> Expr ty ann -> Expr ty ann -> Expr ty ann -> Expr ty ann -> Expr ty ann
pattern Ctor5 a b c d e f = App Nothing (Ctor4 a b c d e) f

instance Plated (Expr ty ann) where
  plate = gplate

makePrisms ''Expr

subst :: String -> Expr ty ann -> Expr ty ann -> Expr ty ann
subst n e = transform fn
  where
     fn a =
       maybe
         a
         (\val -> if snd val == n then e else a)
         (a ^? Phil.Core._Var)

betaReduce :: Expr ty ann -> Expr ty ann
betaReduce (App ann f x) =
  case betaReduce f of
    Abs _ n body -> betaReduce (subst n x body)
    f' -> App ann f' x
betaReduce (Ann ann e ty) = Ann ann (betaReduce e) ty
betaReduce a = a

exprAnn :: Lens' (Expr ty ann) (Maybe ann)
exprAnn =
  lens
    (\case
        Var ann _ -> ann
        Abs ann _ _ -> ann
        App ann _ _ -> ann
        Ann ann _ _ -> ann
        Quote ann _ -> ann
        Unquote ann _ -> ann
        String ann _ -> ann
        Int ann _ -> ann
        Hole ann -> ann)
    (\e ann ->
       case e of
        Var _ a -> Var ann a
        Abs _ a b -> Abs ann a b
        App _ a b -> App ann a b
        Ann _ a b -> Ann ann a b
        Quote _ a -> Quote ann a
        Unquote _ a -> Unquote ann a
        String _ a -> String ann a
        Int _ a -> Int ann a
        Hole _ -> Hole ann)

exprTypes :: Traversal (Expr ty ann) (Expr ty' ann) (ty String) (ty' String)
exprTypes f expr =
  case expr of
    Var ann a -> pure $ Var ann a
    Abs ann a b -> Abs ann a <$> exprTypes f b
    App ann a b -> App ann <$> exprTypes f a <*> exprTypes f b
    Ann ann a b -> Ann ann <$> exprTypes f a <*> f b
    Quote ann a -> Quote ann <$> exprTypes f a
    Unquote ann a -> Unquote ann <$> exprTypes f a
    String ann a -> pure $ String ann a
    Int ann a -> pure $ Int ann a
    Hole ann -> pure $ Hole ann

-- This is a prism
class Quotable q where
  quote :: q -> Expr ty ann
  unquote :: Expr ty ann -> Maybe q

instance Quotable Int64 where
  quote i = Int Nothing i

  unquote (Ann _ e _) = unquote e
  unquote (Int Nothing i) = Just i
  unquote _ = Nothing

instance Quotable ByteString where
  quote b = String Nothing (Char8.unpack b)

  unquote (Ann _ e _) = unquote e
  unquote (String Nothing s) = Just $ Char8.pack s
  unquote _ = Nothing

instance Quotable Delta where
  quote (Columns a b) = Ctor2 "Columns" (quote a) (quote b)
  quote (Tab a b c) = Ctor3 "Tab" (quote a) (quote b) (quote c)
  quote (Lines a b c d) = Ctor4 "Lines" (quote a) (quote b) (quote c) (quote d)
  quote (Directed a b c d e) = Ctor5 "Lines" (quote a) (quote b) (quote c) (quote d) (quote e)

  unquote (Ann _ e _) = unquote e
  unquote (Ctor2 "Columns" a b) = Columns <$> unquote a <*> unquote b
  unquote (Ctor3 "Tab" a b c) = Tab <$> unquote a <*> unquote b <*> unquote c
  unquote (Ctor4 "Lines" a b c d) =
    Lines <$> unquote a <*> unquote b <*> unquote c <*> unquote d
  unquote (Ctor5 "Lines" a b c d e) =
    Directed <$> unquote a <*> unquote b <*> unquote c <*>
    unquote d <*> unquote e
  unquote _ = Nothing

instance Quotable Span where
  quote (Span a b c) = Ctor3 "Span" (quote a) (quote b) (quote c)

  unquote (Ann _ e _) = unquote e
  unquote (Ctor3 "Span" a b c) =
    Span <$> unquote a <*> unquote b <*> unquote c
  unquote _ = Nothing

instance Quotable a => Quotable (Maybe a) where
  quote Nothing = Ctor "Nothing"
  quote (Just a) = Ctor1 "Just" (quote a)

  unquote (Ann _ e _) = unquote e
  unquote (Ctor "Nothing") = Just Nothing
  unquote (Ctor1 "Just" a) = Just <$> unquote a
  unquote _ = Nothing

instance Quotable String where
  quote = String Nothing

  unquote (Ann _ e _) = unquote e
  unquote (String Nothing a) = Just a
  unquote _ = Nothing

instance (Quotable ann, Quotable a) => Quotable (Type ann a) where
  quote e =
    case e of
      TyVar ann a -> Ctor2 "TyVar" (quote ann) (quote a)
      TyArr ann ty1 ty2 -> Ctor3 "TyArr" (quote ann) (quote ty1) (quote ty2)
      TyCtor ann a -> Ctor2 "TyCtor" (quote ann) (quote a)

  unquote (Ann _ e _) = unquote e
  unquote e =
    case e of
      Ctor2 "TyVar" ann a -> TyVar <$> unquote ann <*> unquote a
      Ctor3 "TyArr" ann ty1 ty2 ->
        TyArr <$> unquote ann <*> unquote ty1 <*> unquote ty2
      Ctor2 "TyCtor" ann a -> TyCtor <$> unquote ann <*> unquote a
      _ -> Nothing

instance (Quotable (ty String), Quotable ann) => Quotable (Expr ty ann) where
  quote e =
    case e of
      Var ann name -> Ctor2 "Var" (quote ann) (quote name)
      Abs ann name body -> Ctor3 "Abs" (quote ann) (quote name) (quote body)
      App ann f x -> Ctor3 "App" (quote ann) (quote f) (quote x)
      Hole ann -> Ctor1 "Hole" (quote ann)
      Quote ann e' -> Ctor2 "Quote" (quote ann) (quote e')
      Unquote ann e' -> Ctor2 "Unquote" (quote ann) (quote e')
      String ann a -> Ctor2 "String" (quote ann) (quote a)
      Int ann a -> Ctor2 "Int" (quote ann) (quote a)
      Ann ann e' ty -> Ctor3 "Ann" (quote ann) (quote e') (quote ty)

  unquote (Ann _ e _) = unquote e
  unquote e =
    case e of
      Ctor2 "Var" ann name -> Var <$> unquote ann <*> unquote name
      Ctor3 "Abs" ann name body ->
        Abs <$> unquote ann <*> unquote name <*> unquote body
      Ctor3 "App" ann f x -> App <$> unquote ann <*> unquote f <*> unquote x
      Ctor1 "Hole" ann -> Hole <$> unquote ann
      Ctor2 "Quote" ann e' -> Quote <$> unquote ann <*> unquote e'
      Ctor2 "Unquote" ann e' -> Unquote <$> unquote ann <*> unquote e'
      Ctor2 "String" ann a -> String <$> unquote ann <*> unquote a
      Ctor2 "Int" ann a -> Int <$> unquote ann <*> unquote a
      Ctor3 "Ann" ann e' ty ->
        Ann <$> unquote ann <*> unquote e' <*> unquote ty
      _ -> Nothing
