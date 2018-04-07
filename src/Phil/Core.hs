{-# language DeriveFunctor, DeriveFoldable, DeriveGeneric, DeriveTraversable #-}
{-# language MultiParamTypeClasses, FlexibleInstances #-}
{-# language TemplateHaskell #-}
{-# language LambdaCase #-}
{-# language StandaloneDeriving, UndecidableInstances #-}
module Phil.Core where

import Control.Lens.Lens (Lens', lens)
import Control.Lens.Plated (Plated(..), gplate)
import Control.Lens.Plated1 (Plated1(..))
import Control.Lens.Prism (prism')
import Control.Lens.TH (makePrisms)
import Control.Lens.Traversal (Traversal)
import Control.Monad (ap)
import Control.Monad.Unify (AsVar(..), HasAnnotation(..), Unifiable(..))
import Data.Deriving (deriveOrd1, deriveEq1, deriveShow1)
import GHC.Generics (Generic)

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
  | Ann (Maybe ann) (Expr ty ann) (ty String)
  deriving (Generic)
deriving instance (Eq (ty String), Eq ann) => Eq (Expr ty ann)
deriving instance (Show (ty String), Show ann) => Show (Expr ty ann)

instance Plated (Expr ty ann) where
  plate = gplate

makePrisms ''Expr

exprAnn :: Lens' (Expr ty ann) (Maybe ann)
exprAnn =
  lens
    (\case
        Var ann _ -> ann
        Abs ann _ _ -> ann
        App ann _ _ -> ann
        Ann ann _ _ -> ann
        Hole ann -> ann)
    (\e ann ->
       case e of
        Var _ a -> Var ann a
        Abs _ a b -> Abs ann a b
        App _ a b -> App ann a b
        Ann _ a b -> Ann ann a b
        Hole _ -> Hole ann)

exprTypes :: Traversal (Expr ty ann) (Expr ty' ann) (ty String) (ty' String)
exprTypes f expr =
  case expr of
    Var ann a -> pure $ Var ann a
    Abs ann a b -> Abs ann a <$> exprTypes f b
    App ann a b -> App ann <$> exprTypes f a <*> exprTypes f b
    Ann ann a b -> Ann ann <$> exprTypes f a <*> f b
    Hole ann -> pure $ Hole ann
