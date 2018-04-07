{-# language ScopedTypeVariables #-}
{-# language TemplateHaskell #-}
{-# language MultiParamTypeClasses, FlexibleInstances #-}
{-# language FlexibleContexts #-}
module Phil.Infer where

import Control.Lens.Getter ((^.))
import Control.Lens.Iso (from)
import Control.Lens.Prism (_Left)
import Control.Lens.Review ((#))
import Control.Lens.TH (makePrisms)
import Control.Monad (join)
import Control.Monad.State (evalState, get, put)
import Control.Monad.Trans (lift)
import Control.Monad.Unify
  (UnifyT, UTerm, UVar, runUnifyT, _Var, fresh, unify, find, unfreeze, uterm,
   AsUnificationError(..))
import Data.Map (Map)
import Data.Maybe (fromMaybe)

import qualified Data.Map as Map

import Phil.Core (Expr(..), Type(..))

data TypeError tyAnn ann v
  = NotFound (Maybe ann) String
  | Occurs UVar (UTerm (Type tyAnn) v) tyAnn
  | TypeMismatch (UTerm (Type tyAnn) v) (UTerm (Type tyAnn) v) tyAnn
  deriving Show
makePrisms ''TypeError

instance AsUnificationError (TypeError tyAnn ann v) (Type tyAnn) v tyAnn where
  _OccursError = _Occurs
  _MismatchError = _TypeMismatch

generalize :: UTerm (Type tyAnn) String -> Type tyAnn String
generalize t =
  join $
  evalState (traverse rename (t ^. from uterm)) initialState
  where
    initialState = (("t"++) . show <$> [0..], Map.empty)
    rename (Left uvar) = do
      (name:names, mapping) <- get
      case Map.lookup uvar mapping of
        Nothing -> do
          put (names, Map.insert uvar name mapping)
          pure $ TyVar name
        Just name' -> pure $ TyVar name'
    rename (Right a) = pure $ TyVar a

infer
  :: forall tyAnn ann
   . Ord tyAnn
  => Map String (Type tyAnn String)
  -> Expr ann
  -> Either (TypeError tyAnn ann String) (Type tyAnn String)
infer ctxt expr =
  let
    ctxt' = fmap unfreeze ctxt
  in

    runUnifyT $
      (case expr of
        Ann ann expr' -> go ctxt' (Just ann) expr'
        _ -> go ctxt' Nothing expr) >>=
      find >>=
      pure . generalize

  where
    go
      :: Map String (UTerm (Type tyAnn) String)
      -> Maybe ann
      -> Expr ann
      -> UnifyT
           (Type tyAnn)
           String
           (Either (TypeError tyAnn ann String))
           (UTerm (Type tyAnn) String)
    go ctxt maybeAnn expr =
      case expr of
        Ann ann expr' -> go ctxt (Just ann) expr'
        Var v ->
          case Map.lookup v ctxt of
            Nothing -> lift . Left $ NotFound maybeAnn v
            Just ty -> lift $ Right ty
        Abs n expr' -> do
          tyVar <- (from uterm._Var._Left #) <$> fresh
          retTy <- go (Map.insert n tyVar ctxt) Nothing expr'
          pure $ TyArr (tyVar ^. from uterm) (retTy ^. from uterm) ^. uterm
        App f x -> do
          xTy <- go ctxt Nothing x
          fTy <- go ctxt Nothing f
          tyVar <- (from uterm._Var._Left #) <$> fresh
          unify fTy (TyArr (xTy ^. from uterm) (tyVar ^. from uterm) ^. uterm)
          pure tyVar
