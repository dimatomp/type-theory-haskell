import Control.Lens
import Control.Monad

import Data.Set
import Data.Set.Lens

import LambdaExpression

import System.IO

main = do
    lambda <- liftM parseLambda $ readFile "task2.in"
    hOut <- openFile "task2.out" WriteMode
    forM_ (setOf (coerced freeVars.name) lambda) $ hPutStrLn hOut
    hClose hOut
