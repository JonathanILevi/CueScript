module ScriptPrelude where
import qualified Prelude as P
import Prelude (($), IO)

import Script
import ScriptFunctions as SF

import Data.Map

import qualified VLCBackend

createScriptPrelude :: IO (Script.Stack, ((P.String, Value) -> IO Value))
createScriptPrelude = do
	vlcInstance <- VLCBackend.newInstance
	let	runAction :: (P.String, Value) -> IO Value
		runAction ("",_) = P.return Undefined
		runAction ("print", value) = print value
		runAction ("printPrefix", value) = printPrefix value
		runAction ("input", value) = input value
		runAction ("wait", value) = wait value
		runAction ("play", value) = play vlcInstance value
	
	P.return (fromList	[ ("add"	, Function $ SF.add	)
		, ("print"	, makeArgProcedure "print"	)
		, ("printPrefix"	, makeArgProcedure "printPrefix"	)
		, ("input"	, makeArgProcedure "input"	)
		, ("wait"	, makeArgProcedure "wait"	)
		, ("play"	, makeArgProcedure "play"	)
		], runAction)


makeArgProcedure :: P.String -> Script.Value
makeArgProcedure name = Script.Function $ (\v->Script.Action (name, v))

