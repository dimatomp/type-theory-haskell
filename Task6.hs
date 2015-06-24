import Control.Lens
import Control.Monad.State
import Control.Monad.Writer

import Data.Char
import Data.HashMap.Lazy as H
import Data.Maybe
import Data.List as L

import LambdaExpression
import Unification

type NameGen = WriterT [Equation] (State (Int, HashMap String String))

newVar :: NameGen String
newVar = do
    cur <- _1 <+= 1
    let per = ord 'z' - ord 'a' + 1
        letter = chr $ ord 'a' + cur `rem` per
        number = guard (cur >= per) >> show (cur `div` per)
    return (letter:number)

genType :: LambdaExpr -> NameGen String
genType (Var name) = do
    table <- use _2
    case H.lookup name table of
        Just mentioned -> return mentioned
        Nothing -> do nName <- newVar
                      _2 %= H.insert name nName
                      return nName
genType (Lam par expr) = do
    pName <- newVar
    before <- _2 %%= liftM2 (,) (H.lookup par) (H.insert par pName)
    eName <- genType expr
    when (isJust before) $ _2 %= H.insert par (fromJust before)
    tName <- newVar
    tell [(Pri tName, Pri pName --> Pri eName)]
    return tName
genType (App fs sn) = do
    fName <- genType fs
    sName <- genType sn
    tName <- newVar
    tell [(Pri fName, Pri sName --> Pri tName)]
    return tName

main = do
    expr <- liftM parseLambda $ readFile "task6.in"
    let (name, system) = evalState (runWriterT $ genType expr) (-1, H.empty)
    writeFile "task6.out" $ (++ "\n") $ case unify system >>= L.lookup name of
        Just t -> show t
        Nothing -> "Выражение не имеет типа"
