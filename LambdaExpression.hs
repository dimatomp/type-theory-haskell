module LambdaExpression where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Trans.State
import Data.Char
import Data.Hashable
import Data.Maybe
import Data.Monoid

data LambdaExpr = Var { name :: String }
                | Lam { param :: String, subst :: LambdaExpr }
                | App { first :: LambdaExpr, second :: LambdaExpr }
                deriving Eq

instance Plated LambdaExpr where
    plate f (Lam par s) = Lam par <$> plate f s
    plate f (App fi se) = App <$> plate f fi <*> plate f se
    plate f var         = f var

instance Show LambdaExpr where
    showsPrec _ (Var name) = showString name
    showsPrec n (Lam par expr) = showParen (n > 0) $ showChar '\\' . showString par . showChar '.' . showsPrec 0 expr
    showsPrec n (App f s) = showParen (n > 1) $ showsPrec 1 f . showChar ' ' . showsPrec 2 s

instance Hashable LambdaExpr where
    hashWithSalt p = hashWithSalt p . show

type Parser = StateT String Maybe

allow :: (Char -> Bool) -> Parser Char
allow f = StateT $ \str -> uncons str >>= liftM2 (<$) id (_1 $ guard . f)

char :: Char -> Parser ()
char c = () <$ allow (== c)

lexeme :: Parser String
lexeme = some $ allow $ liftA2 (||) isAlphaNum (== '\'')

spaced = (many (allow isSpace) >>)
spacedChar = spaced . char

lambdaExpr :: Parser LambdaExpr
lambdaExpr = liftM (foldl1 App) $ some $ braces <|> (Var <$> spaced lexeme) <|> parseLam
    where
        parseLam = spaced $ liftM2 Lam (spacedChar '\\' >> spaced lexeme) (spacedChar '.' >> spaced lambdaExpr)
        braces = spacedChar '(' >> spaced lambdaExpr >>= (<$ spacedChar ')')

parseLambda :: String -> LambdaExpr
parseLambda = fst . fromJust . runStateT lambdaExpr

freeVars :: Traversal LambdaExpr (Either String LambdaExpr) String LambdaExpr
freeVars = filterVars []
    where
        canBeSubst l res = case res ^? coerced freeVars . filtered (`elem` l) of
            Nothing -> Right res
            Just str -> Left str
        filterVars l f (Var name)
            | name `elem` l = pure $ Right $ Var name
            | otherwise = canBeSubst l <$> f name
        filterVars l f (Lam par expr) = fmap (Lam par) <$> filterVars (par:l) f expr
        filterVars l f (App fi se) = liftA2 App <$> filterVars l f fi <*> filterVars l f se

lReduce :: String -> LambdaExpr -> LambdaExpr -> Either String LambdaExpr
lReduce f t = over freeVars $ \s -> if s == f then t else Var s
