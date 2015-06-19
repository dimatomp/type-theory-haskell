import Control.Lens

import Data.HashMap

import LambdaExpression
import Unification

type NameGen = State (Int, HashMap String String)

newVar :: NameGen String
newVar = do
    cur <- use _1
    let per = ord 'z' - ord 'a' + 1
        letter = chr $ ord 'a' + cur `rem` per
        number = guard (cur >= per) >> show (cur `div` per)
    _1 += 1
    return letter:number

genType :: LambdaExpr -> NameGen [Equation]
genType (Var name) = return []
genType (Lam par expr) = do
    pName <- newVar
    eName <- newVar

main = do

