module Script where

import qualified Prelude as P
import Prelude (($),(=<<),(<$>),(<*>),IO(..), Functor(..), Applicative(..), Monad(..), Traversable(..))
import qualified Data.Map.Strict as M
import qualified Data.Foldable as B
import qualified Data.List as B
----import qualified System.Directory as D

type Block = [Statement]

data Statement	= Run Expression
	| Assign P.String Statement
data Expression	= Call Expression Expression
	| Literal Value
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
data Procedure	= ForeignProcedure P.String{-String is name of arg-} (Value -> IO Value)
	| NativeProcedure Memory Block

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
	show (ForeignProcedure _ _) = "[ForeignProcedure]"
	show (NativeProcedure _ _) = "[Procedure]"

run :: State -> Procedure -> IO Value
run state (ForeignProcedure argName p) = p (lookup state argName)
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
eval (State memory) (Literal (Procedure (NativeProcedure _ p))) = Procedure (NativeProcedure memory p)
eval state (Literal v) = v
eval state (Ref name) = lookup state name


lookup :: State -> P.String -> Value
lookup (State memory) name = memory M.! name

set :: State -> P.String -> Value -> State
set (State memory) n v = State $ M.insert n v memory
----
----lookupFile name = do
----	filter () <$> D.listDirectory nameAsPath
----	where	namePath = 
----		nameFile = 



