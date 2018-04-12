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

quoteInt64 :: Int64 -> Expr ty ann
quoteInt64 i = Int Nothing i

unquoteInt64 :: Expr ty ann -> Maybe Int64
unquoteInt64 (Ann _ e _) = unquoteInt64 e
unquoteInt64 (Int Nothing i) = Just i
unquoteInt64 _ = Nothing

quoteByteString :: ByteString -> Expr ty ann
quoteByteString b = String Nothing (Char8.unpack b)

unquoteByteString :: Expr ty ann -> Maybe ByteString
unquoteByteString (Ann _ e _) = unquoteByteString e
unquoteByteString (String Nothing s) = Just $ Char8.pack s
unquoteByteString _ = Nothing

quoteDelta :: Delta -> Expr ty ann
quoteDelta (Columns a b) = Ctor2 "Columns" (quoteInt64 a) (quoteInt64 b)
quoteDelta (Tab a b c) =
  Ctor3 "Tab" (quoteInt64 a) (quoteInt64 b) (quoteInt64 c)
quoteDelta (Lines a b c d) =
  Ctor4 "Lines" (quoteInt64 a) (quoteInt64 b) (quoteInt64 c) (quoteInt64 d)
quoteDelta (Directed a b c d e) =
  Ctor5 "Lines"
    (quoteByteString a)
    (quoteInt64 b)
    (quoteInt64 c)
    (quoteInt64 d)
    (quoteInt64 e)

unquoteDelta :: Expr ty ann -> Maybe Delta
unquoteDelta (Ann _ e _) = unquoteDelta e
unquoteDelta (Ctor2 "Columns" a b) =
  Columns <$> unquoteInt64 a <*> unquoteInt64 b
unquoteDelta (Ctor3 "Tab" a b c) =
  Tab <$> unquoteInt64 a <*> unquoteInt64 b <*> unquoteInt64 c
unquoteDelta (Ctor4 "Lines" a b c d) =
  Lines <$>
  unquoteInt64 a <*> unquoteInt64 b <*>
  unquoteInt64 c <*> unquoteInt64 d
unquoteDelta (Ctor5 "Lines" a b c d e) =
  Directed <$>
  unquoteByteString a <*> unquoteInt64 b <*> unquoteInt64 c <*>
  unquoteInt64 d <*> unquoteInt64 e
unquoteDelta _ = Nothing

quoteSpan :: Span -> Expr ty ann
quoteSpan (Span a b c) =
  Ctor3 "Span" (quoteDelta a) (quoteDelta b) (quoteByteString c)

unquoteSpan :: Expr ty ann -> Maybe Span
unquoteSpan (Ann _ e _) = unquoteSpan e
unquoteSpan (Ctor3 "Span" a b c) =
  Span <$> unquoteDelta a <*> unquoteDelta b <*> unquoteByteString c
unquoteSpan _ = Nothing

quoteMaybe :: (a -> Expr ty ann) -> Maybe a -> Expr ty ann
quoteMaybe _ Nothing = Ctor "Nothing"
quoteMaybe f (Just a) = Ctor1 "Just" (f a)

unquoteMaybe :: (Expr ty ann -> Maybe a) -> Expr ty ann -> Maybe (Maybe a)
unquoteMaybe f (Ann _ e _) = unquoteMaybe f e
unquoteMaybe _ (Ctor "Nothing") = Just Nothing
unquoteMaybe f (Ctor1 "Just" a) = Just <$> f a
unquoteMaybe _ _ = Nothing

quoteString :: String -> Expr ty ann
quoteString = String Nothing

unquoteString :: Expr ty ann -> Maybe String
unquoteString (Ann _ e _) = unquoteString e
unquoteString (String Nothing a) = Just a
unquoteString _ = Nothing

quoteType
  :: (ann -> Expr ty ann)
  -> (a -> Expr ty ann)
  -> Type ann a
  -> Expr ty ann
quoteType qAnn qA e =
  case e of
    TyVar ann a -> Ctor2 "TyVar" (quoteMaybe qAnn ann) (qA a)
    TyArr ann ty1 ty2 ->
      Ctor3 "TyArr"
        (quoteMaybe qAnn ann)
        (quoteType qAnn qA ty1)
        (quoteType qAnn qA ty2)
    TyCtor ann a -> Ctor2 "TyCtor" (quoteMaybe qAnn ann) (quoteString a)

unquoteType
  :: (Expr ty ann -> Maybe ann)
  -> (Expr ty ann -> Maybe a)
  -> Expr ty ann
  -> Maybe (Type ann a)
unquoteType uAnn uA (Ann _ e _) = unquoteType uAnn uA e
unquoteType uAnn uA e =
  case e of
    Ctor2 "TyVar" ann a -> TyVar <$> unquoteMaybe uAnn ann <*> uA a
    Ctor3 "TyArr" ann ty1 ty2 ->
      TyArr <$>
      unquoteMaybe uAnn ann <*>
      unquoteType uAnn uA ty1 <*>
      unquoteType uAnn uA ty2
    Ctor2 "TyCtor" ann a ->
      TyCtor <$> unquoteMaybe uAnn ann <*> unquoteString a
    _ -> Nothing

quoteExpr
  :: (ty String -> Expr ty ann)
  -> (ann -> Expr ty ann)
  -> Expr ty ann
  -> Expr ty ann
quoteExpr qTy qAnn e =
  case e of
    Var ann name ->
      Ctor2 "Var" (quoteMaybe qAnn ann) (quoteString name)
    Abs ann name body ->
      Ctor3 "Abs"
        (quoteMaybe qAnn ann)
        (quoteString name)
        (quoteExpr qTy qAnn body)
    App ann f x ->
      Ctor3 "App"
        (quoteMaybe qAnn ann)
        (quoteExpr qTy qAnn f)
        (quoteExpr qTy qAnn x)
    Hole ann ->
      Ctor1 "Hole" (quoteMaybe qAnn ann)
    Quote ann e' ->
      Ctor2 "Quote" (quoteMaybe qAnn ann) (quoteExpr qTy qAnn e')
    Unquote ann e' ->
      Ctor2 "Unquote" (quoteMaybe qAnn ann) (quoteExpr qTy qAnn e')
    String ann a ->
      Ctor2 "String" (quoteMaybe qAnn ann) (quoteString a)
    Int ann a ->
      Ctor2 "Int" (quoteMaybe qAnn ann) (quoteInt64 a)
    Ann ann e' ty ->
      Ctor3 "Ann" (quoteMaybe qAnn ann) (quoteExpr qTy qAnn e') (qTy ty)

unquoteExpr
  :: (Expr ty ann -> Maybe (ty String))
  -> (Expr ty ann -> Maybe ann)
  -> Expr ty ann -> Maybe (Expr ty ann)
unquoteExpr uTy uAnn (Ann _ e _) = unquoteExpr uTy uAnn e
unquoteExpr _ _ (Quote _ e) = Just e
unquoteExpr uTy uAnn e =
  case e of
    Ctor2 "Var" ann name ->
      Var <$> unquoteMaybe uAnn ann <*> unquoteString name
    Ctor3 "Abs" ann name body ->
      Abs <$>
      unquoteMaybe uAnn ann <*>
      unquoteString name <*>
      unquoteExpr uTy uAnn body
    Ctor3 "App" ann f x ->
      App <$>
      unquoteMaybe uAnn ann <*>
      unquoteExpr uTy uAnn f <*>
      unquoteExpr uTy uAnn x
    Ctor1 "Hole" ann ->
      Hole <$> unquoteMaybe uAnn ann
    Ctor2 "Quote" ann e' ->
      Quote <$> unquoteMaybe uAnn ann <*> unquoteExpr uTy uAnn e'
    Ctor2 "Unquote" ann e' ->
      Unquote <$> unquoteMaybe uAnn ann <*> unquoteExpr uTy uAnn e'
    Ctor2 "String" ann a ->
      String <$> unquoteMaybe uAnn ann <*> unquoteString a
    Ctor2 "Int" ann a ->
      Int <$> unquoteMaybe uAnn ann <*> unquoteInt64 a
    Ctor3 "Ann" ann e' ty ->
      Ann <$> unquoteMaybe uAnn ann <*> unquoteExpr uTy uAnn e' <*> uTy ty
    _ -> Nothing

data Definition ann
  = DefTypeSig
  { _definitionAnn :: Maybe ann
  , _unsafeDefTypeSigName :: String
  , _unsafeDefTypeSigType :: TypeScheme ann String
  }
  | DefValue
  { _definitionAnn :: Maybe ann
  , _unsafeDefValueName :: String
  , _unsafeDefValueExpr :: Expr (Type ann) ann
  }
  deriving (Eq, Show, Generic)
