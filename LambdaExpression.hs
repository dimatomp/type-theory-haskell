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

recursively :: Simple Traversal LambdaExpr LambdaExpr
recursively f (Lam par s) = Lam par <$> recursively f s
recursively f (App fi se) = App <$> recursively f fi <*> recursively f se
recursively f var         = f var

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

freeVars :: Traversal LambdaExpr (Maybe LambdaExpr) String LambdaExpr
freeVars = filterVars []
    where
        filterVars l f (Var name)
            | name `elem` l = pure $ Just $ Var name
            | otherwise = (liftA2 (<$) id (guard . canBeSubst)) <$> f name
              where canBeSubst = allOf substVars (`notElem` l)
        filterVars l f (Lam par expr) = filterVars (par:l) f expr
        filterVars l f (App fi se) = (liftA2 App) <$> filterVars l f fi <*> filterVars l f se

-- I still do not know why they have limited the number
-- of parameters in Getting from 5 to 3. It would be so flexible here.
substVars :: Monoid m => Getting m LambdaExpr String
substVars = (coerce .) . (freeVars . (coerce .))

lReduce :: String -> LambdaExpr -> LambdaExpr -> Maybe LambdaExpr
lReduce f t = over freeVars $ \s -> if s == f then t else Var s
