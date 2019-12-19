module ScriptPrelude where
import qualified Prelude as P
import Prelude (($), IO)

import Script
import ScriptFunctions as SF

import Data.Map

import qualified VLCBackend

createScriptPrelude :: IO Script.Stack
createScriptPrelude = do
	vlcInstance <- VLCBackend.newInstance
	P.return $ fromList	[ ("add"	, Function $ SF.add	)
		, ("print"	, makeArgProcedure "print"	)
		]

runAction :: (P.String, Value) -> IO Value
runAction ("print", value) = print value


makeArgProcedure :: P.String -> Script.Value
makeArgProcedure name = Script.Function $ (\v->Script.Action (name, v))

