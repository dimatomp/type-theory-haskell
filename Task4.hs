{-# LANGUAGE DeriveDataTypeable #-}

import Control.Lens
import Control.Monad

import Data.Char
import Data.Data
import Data.Data.Lens
import Data.List
import Data.Maybe

import LambdaExpression

data DeBrujin = DVar { num :: Int }
              | DApp { fst :: DeBrujin, snd :: DeBrujin }
              | DLam { val :: DeBrujin }
              deriving (Data, Show, Eq)

allNums :: Simple Traversal DeBrujin Int
allNums f (DVar num) = DVar <$> f num
allNums f (DApp fi se) = DApp <$> allNums f fi <*> allNums f se
allNums f (DLam v) = DLam <$> allNums f v

dbReduce :: DeBrujin -> DeBrujin
dbReduce (DApp (DLam pat) sub) = dbReduce $ subst 1 pat sub
    where
        subst n (DVar m) sub
            | m == n = sub & allNums +~ (n - 1)
            | otherwise = DVar m
        subst n (DApp f s) sub = subst n f sub `DApp` subst n s sub
        subst n (DLam v) sub = DLam $ subst (n + 1) v sub
dbReduce (DVar n) = DVar n
dbReduce expr = let down = over uniplate dbReduce expr
                in if down == expr then down else dbReduce down

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
