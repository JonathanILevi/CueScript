module Script where

import qualified Prelude as P
import Prelude (($),(=<<),(<$>),(<*>),(++),(==),(||),IO(..), Functor(..), Applicative(..), Monad(..), Traversable(..))
import qualified Data.Map.Strict as M
import qualified Data.Foldable as B
import qualified Data.List as B
import qualified Data.Maybe as B
import qualified Control.Monad as B
import qualified Control.Monad.State as S
import qualified Control.Monad.Reader as R
import qualified System.IO.Unsafe as B
import qualified System.FilePath as D
import qualified System.Directory as D

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
	| List	[Value]
	| Map	(M.Map P.String Value)
----	| Pointer	(IORef Value)
	| Function	(Value -> Value)
	| Action	ActionRef --Always Foreign
	| Procedure	Block


data Literal	= LitUndefined
	| LitNumber	P.Double
	| LitString	P.String
	| LitList	[Value]
	| LitMap	(M.Map P.String Value)
	| LitFunction	P.String Expression
	| LitProcedure	Block
	

type ActionRef = (P.String, Value)

type Stack = M.Map P.String Value

data Breakpoint	= Breakpoint (P.Maybe (ActionRef, (Value -> Breakpoint)))

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

--{ TODO: Rework this with functional `Assign` `Statement`.  and more clear?.
start :: Block -> R.Reader Stack Breakpoint
start ([]) = return $ Breakpoint $ P.Nothing
----start ((Run e):ss) = do
----	stack <- R.ask
----	let ourNext = (\_->R.runReader (start ss) stack)
----	extendBreakpoint ourNext <$> (run =<< eval e)
start (s:ss) = do
	stack <- R.ask
	return $ buildBreakpoint s stack (\stack _->R.runReader (start ss) stack)

buildBreakpoint :: Statement -> Stack -> (Stack->Value->Breakpoint) -> Breakpoint
buildBreakpoint (Run e) stack next = R.runReader (extendBreakpoint (next stack) <$> (run =<< eval e) ) stack
buildBreakpoint (Assign vn s) stack next = buildBreakpoint s stack (\stack v->next (set vn v stack) v)

extendBreakpoint :: (Value -> Breakpoint) -> Breakpoint -> Breakpoint
extendBreakpoint extention (Breakpoint (P.Just (this,next))) = Breakpoint $ P.Just (this, (\v->extendBreakpoint extention (next v)))
extendBreakpoint extention (Breakpoint (P.Nothing)) = extention Undefined

run :: Value -> R.Reader Stack Breakpoint
run (Action action) = return $ Breakpoint $ P.Just (action, (\_->Breakpoint P.Nothing))
run (Procedure block) = start block
run _ = return $ Breakpoint $ P.Just (("",Undefined), (\_->Breakpoint P.Nothing))
--}

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

{-# NOINLINE lookup #-}
lookup :: P.String -> R.Reader Stack Value
lookup name = do
	stack <- R.ask
	return $ P.maybe (P.maybe Undefined P.id $ B.unsafePerformIO $ loadFileValue name) P.id $ M.lookup name stack

loadFileValue :: P.String -> IO (P.Maybe Value)
loadFileValue path = do
	let dir = D.takeDirectory path
	let file = D.takeFileName path
	fileExists <- D.doesFileExist path
	if fileExists then P.Just <$> loadFoundFileValue path
	else do
		dirExists <- D.doesDirectoryExist dir
		if  dirExists then do
			found <- B.filterM (\f->D.doesFileExist (dir D.</> f)) =<< P.filter (\f->file == D.dropExtension f) <$> D.listDirectory (dir)
			if P.length found == 1
				then P.Just <$> loadFoundFileValue (dir D.</> P.head found)
				else (P.print $ "Found "++P.show (P.length found)++" matching files!?") >> P.return (P.Just Undefined)
		else return P.Nothing

loadFoundFileValue :: P.String -> IO Value
loadFoundFileValue name = do
	let ext = D.takeExtension name
	if ext == ".wav" || ext == ".mp3" || ext == ".ogg"
		then return $ Action ("play", String name)
		else return $ Undefined

set ::  P.String -> Value -> Stack -> Stack
set n v stack = M.insert n v stack
----
----lookupFile name = do
----	filter () <$> D.listDirectory nameAsPath
----	where	namePath = 
----		nameFile = 



