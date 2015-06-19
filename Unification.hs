{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}

module Unification where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.Data
import Data.Data.Lens
import Data.Hashable
import Data.List
import Data.Maybe
--import Data.HashSet hiding (map)

data Term = Pri { _vName :: String }
          | Fun { _fName :: String, _args :: [Term] }
          deriving (Eq, Data)
makeLenses ''Term

instance Show Term where
    show (Pri name) = name
    show (Fun name list) = name ++ '(' : intercalate "," (map show list) ++ ")"

instance Hashable Term where
    hashWithSalt p = hashWithSalt p . show

type Equation = (Term, Term)

applyRule1 :: Equation -> Equation -> [Equation]
applyRule1 eq1 eq2 = do
    name1 <- eq1 ^.. _1.vName
    name2 <- eq2 ^.. _1.vName
    guard $ name1 == name2
    [eq1, (eq1 ^. _2, eq2 ^. _2)]

applyRule2 :: Equation -> Equation
applyRule2 (a, b) = (b, a)

applyRule3 :: Equation -> Maybe [Equation]
applyRule3 eq = fmap (fromMaybe []) $ runMaybeT $ do
    name1 <- MaybeT $ return $ eq ^? _1.fName
    name2 <- MaybeT $ return $ eq ^? _2.fName
    lift $ guard $ name1 == name2 && length (eq^._1.args) == length (eq^._2.args)
    return $ zip (eq ^. _1.args) (eq ^. _2.args)

applyRule4 :: Equation -> Equation -> [Equation]
applyRule4 eq1 eq2 = do
    name1 <- eq1 ^.. _1.vName
    let substitute (Pri s) = if s == name1 then eq1 ^. _2 else Pri s
    [eq1, over (both.uniplate) substitute eq2]

applyRule5 :: [Equation] -> [Equation]
applyRule5 list = list ^.. traverse . filtered (\(a, b) -> a /= b)

applyRule6 :: Equation -> Maybe ()
applyRule6 eq = fmap (fromMaybe ()) $ runMaybeT $ do
    name <- MaybeT $ return $ eq ^? _1.vName
    args <- MaybeT $ return $ eq ^? _2.args
    lift $ guard $ notElemOf (traverse.uniplate.vName) name args
