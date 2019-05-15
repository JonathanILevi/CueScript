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

scriptPrelude = fromList	[ (":", Script.Function $ wait)
	, ("wait", Script.Function $ wait)
	, ("play", Script.Function $ play)
	, ("print", Script.Function $ print)
	, ("printPrefix", Script.Function $ printPrefix)
	, ("input", Script.Function $ input)
	]


