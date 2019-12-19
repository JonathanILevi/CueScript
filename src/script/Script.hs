module Script where

import qualified Prelude as P
import Prelude (($),(=<<),(<$>),(<*>),(++),IO(..), Functor(..), Applicative(..), Monad(..), Traversable(..))
import qualified Data.Map.Strict as M
import qualified Data.Foldable as B
import qualified Data.List as B
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
----import qualified System.Directory as D

type Block = [Statement]

data Statement	= Run Expression
	| Assign P.String Statement
data Expression	= Call Expression Expression
	| Literal Literal
	| Ref P.String
	deriving (P.Show)

data Value	= Undefined	
	| Number	P.Double
	| String	P.String
	| Function	(Value -> Value)
	| Procedure	(IO Value)
	| List	[Value]
	| Map	(M.Map P.String Value)
	| Pointer	IORef Value ----Unimplemented


data Literal	= LitUndefined
	| LitNumber	P.Double
	| LitString	P.String
	| LitFunction	P.String Expression
	| LitProcedure	Block
	| LitList	[Value]
	| LitMap	(M.Map P.String Value)
	

type Stack = M.Map P.String Value

instance P.Show Value where
	show Undefined	= "[Undefined]"
	show (Number v)	= P.show v
	show (String v)	= v
	show (Function v)	= "[Function]"
	show (Procedure v)	= "[Procedure]"
	show (List v)	= P.show v
	show (Map v)	= P.show v
instance P.Show Literal where
	show LitUndefined	= "[Literal]" ++ "[Undefined]"
	show (LitNumber v)	= "[Literal]" ++ P.show v
	show (LitString v)	= "[Literal]" ++ v
	show (LitFunction _ _)	= "[Literal]" ++ "[Function]"
	show (LitProcedure _)	= "[Literal]" ++ "[Procedure]"
	show (LitList v)	= "[Literal]" ++ P.show v
	show (LitMap v)	= "[Literal]" ++ P.show v



eval :: Expression -> R.Reader Stack Value
eval (Call fe ae) = call <$> eval fe <*> eval ae
	where	call :: Value -> Value -> Value
		call (Function f) v = f v
eval (Literal l) = makeLiteral l
eval (Ref name) = lookup name

makeLiteral :: Literal -> R.Reader Stack Value
makeLiteral (LitUndefined) = return $ Undefined
makeLiteral (LitNumber v) = return $ Number v
makeLiteral (LitString v) = return $ String v
makeLiteral (LitFunction argName body) = do
	stack <- R.ask
	return $ Function (\arg->R.runReader (eval body) (set argName arg stack))
makeLiteral (LitList vs) = return $ List vs
makeLiteral (LitMap v) = return $ Map v


lookup :: P.String -> R.Reader Stack Value
lookup name = (M.!) <$> R.ask <*> pure name

set ::  P.String -> Value -> Stack -> Stack
set n v stack = M.insert n v stack
----
----lookupFile name = do
----	filter () <$> D.listDirectory nameAsPath
----	where	namePath = 
----		nameFile = 



