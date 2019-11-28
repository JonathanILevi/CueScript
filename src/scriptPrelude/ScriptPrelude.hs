module ScriptPrelude where
import qualified Prelude as P
import Prelude (($), IO)

import Script
import ScriptFunctions

import Data.Map

import qualified VLCBackend

createScriptPrelude :: IO Script.Memory
createScriptPrelude = do
	vlcInstance <- VLCBackend.newInstance
	P.return $ fromList	[ (":"	, makeForeignProcedure $ wait	)
		, ("wait"	, makeForeignProcedure $ wait	)
		, ("play"	, makeForeignProcedure $ play vlcInstance	)
		, ("print"	, makeForeignProcedure $ print	)
		, ("printPrefix"	, makeForeignProcedure $ printPrefix	)
		, ("input"	, makeForeignProcedure $ input	)
		]


makeForeignProcedure :: (Script.Value -> IO Script.Value) -> Script.Value
makeForeignProcedure p = Script.Function $ Script.ForeignFunction (\value->Script.Procedure $ ForeignProcedure $ p value)

