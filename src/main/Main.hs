module Main where

import Script (Procedure,run)
import qualified Script
import MakeScript (script)
import ScriptPrelude
import Data.Map

main = do
	prelude <- createScriptPrelude
	run (Script.State prelude) $ script prelude


