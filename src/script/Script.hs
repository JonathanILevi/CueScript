module Script (Script(..),Statement(..),Value(..),ValueMap, evaluate, eval) where

import qualified Prelude as P
import Prelude (($),(=<<),IO(..), Functor(..), Applicative(..), Monad(..), Traversable(..))
import qualified Data.Map as M

type Script = [Statement]
data Statement = Call P.String [Statement] | Value Value
	deriving (P.Show)
data Value = String P.String | Function ([Value] -> IO Value) | Undefined
type ValueMap = M.Map P.String Value

instance P.Show Value where
	show (String s) = P.show s
	show (Function f) = "[function]"
	show Undefined = "Undefined"



evaluate :: ValueMap -> [Statement] -> IO [Value]
evaluate valueMap ss = sequence $ fmap (eval valueMap) ss

eval :: ValueMap -> Statement -> IO Value
eval valueMap (Call f args) = call valueMap f =<< evaluate valueMap args
eval valueMap (Value v) = return v

call :: ValueMap -> P.String -> [Value] -> IO Value
call valueMap fn args = do
	P.print (fn P.++ " called with: " P.++ P.show args)
	case (valueMap M.! fn) of
		Function f -> f args
