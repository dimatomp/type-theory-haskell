{-# LANGUAGE DeriveDataTypeable #-}

import Control.Lens
import Control.Monad

import Data.Char
import Data.Data
import Data.Data.Lens
import Data.List
import Data.Maybe

--import Debug.Trace{-trace ("!!!" ++ show n ++ ' ':show m) $ -}

import LambdaExpression hiding (subst)

data DeBrujin = DVar { num :: Int }
              | DApp { fst :: DeBrujin, snd :: DeBrujin }
              | DLam { val :: DeBrujin }
              deriving (Typeable, Data, Eq)

instance Show DeBrujin where
    show (DVar num) = show num
    show (DApp f s) = '(':show f ++ ' ':show s ++ ")"
    show (DLam par) = '\\':show par

dFreeVars :: Simple Traversal DeBrujin Int
dFreeVars = apply 0
    where
        apply n f (DVar num)
            | num > n = DVar <$> f num
            | otherwise = pure $ DVar num
        apply n f (DApp fi se) = DApp <$> apply n f fi <*> apply n f se
        apply n f (DLam v) = DLam <$> apply (n + 1) f v

subst n (DVar m) sub = if m == n then sub & dFreeVars +~ n else DVar m
subst n (DApp f s) sub = subst n f sub `DApp` subst n s sub
subst n (DLam v) sub = DLam $ subst (n + 1) v sub

dbReduce :: DeBrujin -> DeBrujin
dbReduce (DApp (DLam pat) sub) = subst 1 pat sub & dFreeVars -~ 1 & dbReduce
dbReduce expr@(DApp f s) = let first = dbReduce f
                           in if f == first then DApp first $ dbReduce s
                                            else dbReduce $ DApp first s
dbReduce (DVar n) = DVar n
dbReduce (DLam v) = DLam $ dbReduce v

chBr :: LambdaExpr -> DeBrujin
chBr expr = convert (expr ^.. coerced freeVars.name) expr
    where
        convert st (Var name) = DVar $ 1 + fromJust (findIndex (== name) st)
        convert st (Lam par sub) = DLam $ convert (par:st) sub
        convert st (App f s) = DApp (convert st f) (convert st s)

brCh :: DeBrujin -> LambdaExpr
brCh expr = convert (max 0 $ maxDep 0 expr) expr
    where
        intToCh i = let intv = ord 'z' - ord 'a' + 1
                        letter = chr $ ord 'a' + (i `rem` intv)
                        num = guard (i >= intv) >> show (i `div` intv)
                    in letter:num
        maxDep d (DVar num) = num - d
        maxDep d (DApp f s) = max (maxDep d f) (maxDep d s)
        maxDep d (DLam v) = maxDep (d + 1) v
        convert dep (DVar num) = Var $ intToCh $ dep - num
        convert dep (DApp f s) = App (convert dep f) (convert dep s)
        convert dep (DLam v) = Lam (intToCh $ dep) $ convert (dep + 1) v

churchBrujin :: Simple Iso LambdaExpr DeBrujin
churchBrujin = iso chBr brCh

main = do
    expr <- liftM parseLambda $ readFile "task4.in"
    writeFile "task4.out" $ show $ over churchBrujin dbReduce expr
