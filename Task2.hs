import Control.Lens
import Control.Monad

import Data.Set

import LambdaExpression

import System.IO

main = do
    lambda <- liftM parseLambda $ readFile "task2.in"
    hOut <- openFile "task2.out" WriteMode
    forM_ (foldMapOf substVars singleton lambda) $ hPutStrLn hOut
    hClose hOut
