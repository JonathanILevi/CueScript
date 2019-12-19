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
		]


