module Script (Script(..),Expression(..),Value(..),Function(..),ScopeVars, eval,run) where

import qualified Prelude as P
import Prelude (($),(=<<),IO(..), Functor(..), Applicative(..), Monad(..), Traversable(..))
import qualified Data.Map.Strict as M
import qualified Data.Foldable as B
import qualified System.Directory as D

data Expression	= Call P.String [Expression]
	| Literal Value
	deriving (P.Show)

data Value	= Undefined	
	| Number	P.Double
	| String	P.String
	| Function	Function
	| List	[Value]
	| Map	(M.Map P.String Value)

data Function	= ForeignFunction ([Value] -> IO Value)
	| NativeFunction [P.String] [Expression]

type Script = Function
type ScopeVars = M.Map P.String Value

instance P.Show Value where
	show Undefined	= "[Undefined]"
	show (Number v)	= P.show v
	show (String v)	= v
	show (Function v)	= P.show v
	show (List v)	= P.show v
	show (Map v)	= P.show v
instance P.Show Function where
	show (ForeignFunction _) = "[ForeignFunction]"
	show (NativeFunction _ _) = "[Function]"


eval :: ScopeVars -> Expression -> IO Value
eval scopeVars (Call f args) = do
	f' <- lookup scopeVars f
	args' <- evalSequence scopeVars args
	call scopeVars f' args'
eval scopeVars (Literal v) = return v

run :: ScopeVars -> Function -> [Value] -> IO Value
run scopeVars (ForeignFunction f) args = do
	f args
run scopeVars (NativeFunction a f) args = do
	evalSequence scopeVars f
	return Undefined

evalSequence :: ScopeVars -> [Expression] -> IO [Value]
evalSequence scopeVars ss = sequence $ fmap (eval scopeVars) ss

call :: ScopeVars -> Value -> [Value] -> IO Value
call scopeVars (Function f) args = do
	run scopeVars f args
call scopeVars value [] = do
	return value

lookup :: ScopeVars -> P.String -> IO Value
lookup scopeVars name = do
	return (scopeVars M.! name)



