{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses, FlexibleInstances #-}
{-# language FlexibleContexts #-}
module Phil.Typecheck where

import Control.Lens.Getter ((^.), view)
import Control.Lens.Iso (from)
import Control.Lens.Prism (_Left)
import Control.Lens.Review ((#))
import Control.Lens.TH (makePrisms)
import Control.Monad ((<=<), join)
import Control.Monad.State (runState, get, put)
import Control.Monad.Trans (lift)
import Control.Monad.Unify
  (UnifyT, UTerm, UVar, runUnifyT, _Var, fresh, unify, find, unfreeze, uterm,
   AsUnificationError(..))
import Data.Map (Map)

import qualified Data.Map as Map

import Phil.Core (Expr(..), Type(..), TypeScheme(..))

data TypeError tyAnn ann v
  = NotFound (Maybe ann) String
  | Occurs UVar (UTerm (Type tyAnn) v) (Maybe tyAnn)
  | TypeMismatch (UTerm (Type tyAnn) v) (UTerm (Type tyAnn) v) (Maybe tyAnn)
  deriving Show
makePrisms ''TypeError

instance AsUnificationError (TypeError tyAnn ann v) (Type tyAnn) v (Maybe tyAnn) where
  _OccursError = _Occurs
  _MismatchError = _TypeMismatch

generalize :: UTerm (Type tyAnn) String -> TypeScheme tyAnn String
generalize t =
  let
    (res, (_, mapping)) =
      runState (traverse rename (t ^. from uterm)) initialState
  in
    Forall (snd <$> reverse mapping) (join res)

  where
    initialState = (("t"++) . show <$> [0::Integer ..], [])
    rename (Left uvar) = do
      (name:names, mapping) <- get
      case lookup uvar mapping of
        Nothing -> do
          put (names, (uvar, name) : mapping)
          pure $ TyVar Nothing name
        Just name' -> pure $ TyVar Nothing name'
    rename (Right a) = pure $ TyVar Nothing a

instantiate
  :: (Monad m, Eq v)
  => TypeScheme ann v
  -> UnifyT (Type ann) v m (UTerm (Type ann) v)
instantiate (Forall vs ty) = do
  mapping <- zip vs <$> traverse (const fresh) vs
  pure . view uterm $
    ty >>= \var -> TyVar Nothing (maybe (Right var) Left (lookup var mapping))

-- | Check that the first argument is an instantiation of the second
--
-- i.e.
-- 
-- check (Int -> Int) (forall a. a -> a) succeeds
-- check (forall a. a -> a) (Int -> Int) fails
check
  :: (Ord v, Ord tyAnn)
  => TypeScheme tyAnn v
  -> TypeScheme tyAnn v
  -> Either (TypeError tyAnn ann v) ()
check (Forall _ ty1) tys2 =
  runUnifyT $ do
    ty1' <- pure $ unfreeze ty1
    ty2' <- instantiate tys2
    unify ty1' ty2'

infer
  :: forall tyAnn ann
   . Ord tyAnn
  => Map String (TypeScheme tyAnn String)
  -> Expr ann
  -> Either (TypeError tyAnn ann String) (TypeScheme tyAnn String)
infer ctxt =
  runUnifyT .
  (pure . generalize <=<
   find <=<
   go Map.empty)

  where
    go
      :: Map String (UTerm (Type tyAnn) String)
      -> Expr ann
      -> UnifyT
           (Type tyAnn)
           String
           (Either (TypeError tyAnn ann String))
           (UTerm (Type tyAnn) String)
    go localCtxt expr =
      case expr of
        Var maybeAnn v ->
          case Map.lookup v localCtxt of
            Just ty -> lift $ Right ty
            Nothing ->
              case Map.lookup v ctxt of
                Nothing -> lift . Left $ NotFound maybeAnn v
                Just ty -> instantiate ty >>= lift . Right
        Abs _ n expr' -> do
          tyVar <- (from uterm._Var._Left #) <$> fresh
          retTy <- go (Map.insert n tyVar localCtxt) expr'
          pure $ TyArr Nothing (tyVar ^. from uterm) (retTy ^. from uterm) ^. uterm
        App _ f x -> do
          xTy <- go localCtxt x
          fTy <- go localCtxt f
          tyVar <- (from uterm._Var._Left #) <$> fresh
          unify fTy (TyArr Nothing (xTy ^. from uterm) (tyVar ^. from uterm) ^. uterm)
          pure tyVar
