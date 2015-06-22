{-# LANGUAGE TemplateHaskell #-}

module LambdaExpression where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.State

import Data.Hashable
import Data.Maybe

import Parser

data LambdaExpr = Var { _name :: String }
                | Lam { param :: String, subst :: LambdaExpr }
                | App { first :: LambdaExpr, second :: LambdaExpr }
                deriving Eq
makeLenses ''LambdaExpr

instance Show LambdaExpr where
    showsPrec _ (Var name) = showString name
    showsPrec n (Lam par expr) = showParen (n > 0) $ showChar '\\' . showString par . showChar '.' . showsPrec 0 expr
    showsPrec n (App f s) = showParen (n > 1) $ showsPrec 1 f . showChar ' ' . showsPrec 2 s

instance Hashable LambdaExpr where
    hashWithSalt p = hashWithSalt p . show

lambdaExpr :: Parser LambdaExpr
lambdaExpr = liftM (foldl1 App) $ some $ braces <|> (Var <$> spaced lexeme) <|> parseLam
    where
        parseLam = spaced $ liftM2 Lam (spacedChar '\\' >> spaced lexeme) (spacedChar '.' >> spaced lambdaExpr)
        braces = spacedChar '(' >> spaced lambdaExpr >>= (<$ spacedChar ')')

parseLambda :: String -> LambdaExpr
parseLambda = fst . fromJust . runStateT lambdaExpr

freeVars :: Traversal LambdaExpr (Either String LambdaExpr) LambdaExpr LambdaExpr
freeVars = filterVars []
    where
        canBeSubst l res = case res ^? coerced freeVars . name . filtered (`elem` l) of
            Nothing -> Right res
            Just str -> Left str
        filterVars l f (Var name)
            | name `elem` l = pure $ Right $ Var name
            | otherwise = canBeSubst l <$> f (Var name)
        filterVars l f (Lam par expr) = fmap (Lam par) <$> filterVars (par:l) f expr
        filterVars l f (App fi se) = liftA2 App <$> filterVars l f fi <*> filterVars l f se

lReduce :: String -> LambdaExpr -> LambdaExpr -> Either String LambdaExpr
lReduce f t = freeVars.filtered((== f).view name) .~ t
