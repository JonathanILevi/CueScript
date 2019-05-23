module Script (Script(..),Expression(..),Value(..),ValueMap, evaluate, eval) where

import qualified Prelude as P
import Prelude (($),(=<<),IO(..), Functor(..), Applicative(..), Monad(..), Traversable(..))
import qualified Data.Map.Strict as M

type Script = [Expression]

data Expression	= Call P.String [Expression]
	| Literal Value
	deriving (P.Show)

data Value	= Undefined	
	| Number	P.Double
	| String	P.String
	| Function	([Value] -> IO Value)
	| List	[Value]
	| Map	(M.Map P.String Value)

type ValueMap = M.Map P.String Value

instance P.Show Value where
	show Undefined	= "[Undefined]"
	show (Number v)	= P.show v
	show (String v)	= v
	show (Function _)	= "[function]"
	show (List v)	= P.show v
	show (Map v)	= P.show v



evaluate :: ValueMap -> [Expression] -> IO [Value]
evaluate valueMap ss = sequence $ fmap (eval valueMap) ss

eval :: ValueMap -> Expression -> IO Value
eval valueMap (Call f args) = call valueMap f =<< evaluate valueMap args
eval valueMap (Literal v) = return v

call :: ValueMap -> P.String -> [Value] -> IO Value
call valueMap fn args = do
	----P.print (fn P.++ " called with: " P.++ P.show args)
	case (valueMap M.! fn) of
		Function f -> f args
