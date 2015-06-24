{-# LANGUAGE TemplateHaskell #-}

module Unification (Term(..), (-->), Equation, unify) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.List
import Control.Monad.Trans
import Control.Monad.Trans.Maybe

import Data.Hashable
import Data.List
import Data.Maybe

import qualified Data.HashSet as H

data Term = Pri { _vName :: String }
          | Fun { _fName :: String, _args :: [Term] }
          deriving Eq
makeLenses ''Term

infixr 1 -->
a --> b = Fun "->" [a, b]

vars :: Simple Traversal Term Term
vars f (Fun fName args) = Fun fName <$> (traverse.vars) f args
vars f pri              = f pri

instance Show Term where
    showsPrec _ (Pri name) = showString name
    showsPrec n (Fun "->" [l, r]) = showParen (n > 0) $ showsPrec 1 l . showString "->" . showsPrec 0 r
    showsPrec _ (Fun name list) = showString name . showChar '(' . foldr (\a b -> shows a . showChar ',' . b) (shows $ last list) (init list) . showChar ')'

instance Hashable Term where
    hashWithSalt p = hashWithSalt p . show

type Equation = (Term, Term)

type Replacement = (Equation, [Equation])
type TryReplace = Maybe (Maybe Replacement)

rule1 :: Equation -> Equation -> TryReplace
rule1 eq1 eq2 = Just $ do
    name1 <- eq1 ^? _1.vName
    name2 <- eq2 ^? _1.vName
    guard $ eq1 /= eq2 && name1 == name2
    return (eq2, [(eq1 ^. _2, eq2 ^. _2)])

rule2 :: Equation -> TryReplace
rule2 (a, b) = Just $ ((a, b), [(b, a)]) <$ (b ^? vName >> a ^? fName)

rule3 :: Equation -> TryReplace
rule3 eq = runMaybeT $ do
    name1 <- MaybeT $ return $ eq ^? _1.fName
    name2 <- MaybeT $ return $ eq ^? _2.fName
    lift $ guard $ name1 == name2 && length (eq^._1.args) == length (eq^._2.args)
    return (eq, zip (eq ^. _1.args) (eq ^. _2.args))

rule4 :: Equation -> Equation -> TryReplace
rule4 eq1 eq2 = Just $ do
    name1 <- eq1 ^? _1.vName
    guard $ eq1 /= eq2 && elemOf (both.vars.vName) name1 eq2
    return (eq2, [eq2 & both.vars.filtered((== name1).view vName) .~ eq1 ^. _2])

rule5 :: Equation -> TryReplace
rule5 (a, b) = Just $ ((a, b), []) <$ guard (a == b)

rule6 :: Equation -> TryReplace
rule6 eq = (Nothing <$) $ runMaybeT $ do
    name <- MaybeT $ return $ eq ^? _1.vName
    args <- MaybeT $ return $ eq ^? _2.args
    lift $ guard $ notElemOf (traverse.vars.vName) name args

rules :: [Equation] -> TryReplace
rules elemList = liftM (listToMaybe . catMaybes) $ runListT $ do
    eq1 <- ListT $ return elemList
    eq2 <- ListT $ return elemList
    let single = map ($ eq1) [rule2, rule3, rule5, rule6]
        double = map (($ eq2) . ($ eq1)) [rule1, rule4]
    lift $ liftM (foldl1 (<|>)) $ sequence $ single ++ double

unify :: [Equation] -> Maybe [(String, Term)]
unify list =
    let repeatStep ht = rules (H.toList ht) >>= \repl -> case repl of
            Nothing -> return ht
            Just (from, to) -> repeatStep $ foldr H.insert (H.delete from ht) to
    in fmap ((map $ _1 %~ view vName).H.toList) $ repeatStep $ H.fromList list
