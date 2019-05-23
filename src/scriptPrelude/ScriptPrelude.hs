module ScriptPrelude where
import qualified Prelude as P
import Prelude (($), IO)

import Script
import ScriptFunctions

import Data.Map

import qualified VLCBackend

setup :: IO ()
setup = VLCBackend.setup
cleanUp :: IO ()
cleanUp = VLCBackend.cleanUp

scriptPrelude = fromList	[ (":"	, Script.Function $ Script.ForeignFunction $ wait	)
	, ("wait"	, Script.Function $ Script.ForeignFunction $ wait	)
	, ("play"	, Script.Function $ Script.ForeignFunction $ play	)
	, ("print"	, Script.Function $ Script.ForeignFunction $ print	)
	, ("printPrefix"	, Script.Function $ Script.ForeignFunction $ printPrefix	)
	, ("input"	, Script.Function $ Script.ForeignFunction $ input	)
	]


