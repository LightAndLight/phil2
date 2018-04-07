{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses, FlexibleInstances #-}
{-# language FlexibleContexts #-}
module Phil.Typecheck where

import Control.Lens.Getter ((^.), view)
import Control.Lens.Iso (from)
import Control.Lens.TH (makePrisms)
import Control.Lens.Traversal (traverseOf)
import Control.Monad (join)
import Control.Monad.State (runState, evalState, get, put)
import Control.Monad.Trans (lift)
import Control.Monad.Unify
  (UnifyT, UTerm, UVar, runUnifyT, fresh, freshVar, unify, find,
   unfreeze, uterm, AsUnificationError(..))
import Data.List ((\\))
import Data.Map (Map)

import qualified Data.Map as Map

import Phil.Core (Expr(..), Type(..), TypeScheme(..), exprTypes)

data TypeError ann v
  = NotFound (Maybe ann) String
  | Occurs UVar (UTerm (Type ann) v) (Maybe ann)
  | TypeMismatch (UTerm (Type ann) v) (UTerm (Type ann) v) (Maybe ann)
  deriving Show
makePrisms ''TypeError

instance AsUnificationError (TypeError ann v) (Type ann) v (Maybe ann) where
  _OccursError = _Occurs
  _MismatchError = _TypeMismatch

generalizeWith
  :: [(UVar, String)]
  -> UTerm (Type ann) String
  -> (Type ann String)
generalizeWith mapping t =
  let
    res = evalState (traverse rename (t ^. from uterm)) initialState
  in
    join res

  where
    initialState =
      ((("t"++) . show <$> [0::Integer ..]) \\ fmap snd mapping, mapping)

    rename (Left uvar) = do
      (name:names, mapping') <- get
      case lookup uvar mapping' of
        Nothing -> do
          put (names, (uvar, name) : mapping')
          pure $ TyVar Nothing name
        Just name' -> pure $ TyVar Nothing name'
    rename (Right a) = pure $ TyVar Nothing a

generalize
  :: UTerm (Type ann) String
  -> ([(UVar, String)], TypeScheme ann String)
generalize t =
  let
    (res, (_, mapping)) =
      runState (traverse rename (t ^. from uterm)) initialState
  in
    (mapping, Forall Nothing (snd <$> reverse mapping) (join res))

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
instantiate (Forall _ vs ty) = do
  mapping <- zip vs <$> traverse (const freshVar) vs
  pure . view uterm $
    ty >>= \var -> TyVar Nothing (maybe (Right var) Left (lookup var mapping))

-- | Check that the first argument is an instantiation of the second
--
-- i.e.
-- 
-- check (Int -> Int) (forall a. a -> a) succeeds
-- check (forall a. a -> a) (Int -> Int) fails
check
  :: (Ord v, Ord ann)
  => TypeScheme ann v
  -> TypeScheme ann v
  -> Either (TypeError ann v) ()
check (Forall _ _ ty1) tys2 =
  runUnifyT $ do
    ty1' <- pure $ unfreeze ty1
    ty2' <- instantiate tys2
    unify ty1' ty2'

infer
  :: forall ann
   . Ord ann
  => Map String (TypeScheme ann String)
  -> Expr (Type ann) ann
  -> Either
       (TypeError ann String)
       (Expr (Type ann) ann, TypeScheme ann String)
infer ctxt e =
  runUnifyT $ do
    (exprRes, tyRes) <- go Map.empty e
    tyRes' <- find tyRes
    let (mapping, tyRes'') = generalize tyRes'
    exprRes' <-
      traverseOf exprTypes (fmap (generalizeWith mapping) . find) exprRes
    pure (exprRes', tyRes'')

  where
    go
      :: Map String (UTerm (Type ann) String)
      -> Expr (Type ann) ann
      -> UnifyT
           (Type ann)
           String
           (Either (TypeError ann String))
           (Expr (UTerm (Type ann)) ann, UTerm (Type ann) String)
    go localCtxt expr =
      case expr of
        Ann ann x ty -> do
          (x', xTy) <- go localCtxt x
          let ty' = unfreeze ty
          unify ty' xTy
          pure (Ann ann x' ty', xTy)
        Var ann v ->
          case Map.lookup v localCtxt of
            Just ty -> pure (Ann Nothing (Var ann v) ty, ty)
            Nothing ->
              case Map.lookup v ctxt of
                Nothing -> lift . Left $ NotFound ann v
                Just ty -> do
                  ty' <- instantiate ty
                  pure (Ann Nothing (Var ann v) ty', ty')
        Abs ann n expr' -> do
          tyVar <- fresh
          (expr'', retTy) <- go (Map.insert n tyVar localCtxt) expr'
          let ty = TyArr Nothing (tyVar ^. from uterm) (retTy ^. from uterm) ^. uterm
          pure (Ann Nothing (Abs ann n expr'') ty, ty)
        App ann f x -> do
          (x', xTy) <- go localCtxt x
          (f', fTy) <- go localCtxt f
          tyVar <- fresh
          unify fTy (TyArr Nothing (xTy ^. from uterm) (tyVar ^. from uterm) ^. uterm)
          pure (Ann Nothing (App ann f' x') tyVar, tyVar)
        Hole ann -> do
          ty <- fresh
          pure (Ann Nothing (Hole ann) ty, ty)
