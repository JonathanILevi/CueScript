module Script where

import qualified Prelude as P
import Prelude (($),(=<<),(<$>),(<*>),(++),IO(..), Functor(..), Applicative(..), Monad(..), Traversable(..))
import qualified Data.Map.Strict as M
import qualified Data.Foldable as B
import qualified Data.List as B
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
	| Function	Function
	| Procedure	Procedure
	| List	[Value]
	| Map	(M.Map P.String Value)

data Function	= ForeignFunction (Value -> Value)
----	| ForeignOperator (Value -> Value -> Value)
	| NativeFunction P.String Expression
----	| NativeOperator P.String P.String Expression
data Procedure	= ForeignProcedure (IO Value)
	| NativeProcedure Memory Block


data Literal	= LitUndefined
	| LitNumber	P.Double
	| LitString	P.String
	| LitFunction	P.String Expression
	| LitProcedure	Block
	| LitList	[Value]
	| LitMap	(M.Map P.String Value)
	

data State =	State
	Memory
type Memory = M.Map P.String Value

instance P.Show Value where
	show Undefined	= "[Undefined]"
	show (Number v)	= P.show v
	show (String v)	= v
	show (Function v)	= P.show v
	show (Procedure v)	= P.show v
	show (List v)	= P.show v
	show (Map v)	= P.show v
instance P.Show Function where
	show (ForeignFunction _) = "[ForeignFunction]"
----	show (ForeignOperator _) = "[ForeignOperator]"
	show (NativeFunction _ _) = "[Function]"
----	show (NativeOperator _) = "[Operator]"
instance P.Show Procedure where
	show (ForeignProcedure _) = "[ForeignProcedure]"
	show (NativeProcedure _ _) = "[Procedure]"
instance P.Show Literal where
	show LitUndefined	= "[Literal]" ++ "[Undefined]"
	show (LitNumber v)	= "[Literal]" ++ P.show v
	show (LitString v)	= "[Literal]" ++ v
	show (LitFunction _ _)	= "[Literal]" ++ "[Function]"
	show (LitProcedure _)	= "[Literal]" ++ "[Procedure]"
	show (LitList v)	= "[Literal]" ++ P.show v
	show (LitMap v)	= "[Literal]" ++ P.show v

run :: State -> Procedure -> IO Value
run state (ForeignProcedure p) = p
run state (NativeProcedure memory block) = P.snd <$> B.foldlM (\(ns, rd) smt->enact ns smt) (State memory, Undefined) block

tryRun :: State -> Value -> IO Value
tryRun state (Procedure p) = run state p
tryRun state (Function f) = tryRun state $ call state f Undefined

call :: State -> Function -> Value -> Value
call state (ForeignFunction f) v = f v
call state (NativeFunction argName expression) v = eval (set state argName v) expression

tryCall :: State -> Value -> Value -> Value
tryCall state (Function f) = call state f

enact :: State -> Statement -> IO (State, Value)
enact state (Run e) = (,) <$> pure state <*> (tryRun state $ eval state e)
enact state (Assign n s) = do
	(nstate,v) <- (enact state s)
	return (set nstate n v, v)

eval :: State -> Expression -> Value
eval state (Call fe ae) = tryCall state (eval state fe) (eval state ae)
eval state (Literal l) = makeLiteral state l
eval state (Ref name) = lookup state name

makeLiteral :: State -> Literal -> Value
makeLiteral state (LitUndefined) = Undefined
makeLiteral state (LitNumber v) = Number v
makeLiteral state (LitString v) = String v
makeLiteral state (LitFunction arg body) = Function $ NativeFunction arg body
makeLiteral (State memory) (LitProcedure p) = Procedure (NativeProcedure memory p)
makeLiteral state (LitList vs) = List vs
makeLiteral state (LitMap v) = Map v


lookup :: State -> P.String -> Value
lookup (State memory) name = memory M.! name

set :: State -> P.String -> Value -> State
set (State memory) n v = State $ M.insert n v memory
----
----lookupFile name = do
----	filter () <$> D.listDirectory nameAsPath
----	where	namePath = 
----		nameFile = 



