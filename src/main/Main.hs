module Main where

import Script (Script,run)
import qualified Script
import MakeScript (script)
import ScriptPrelude
import Data.Map

main = do
	ScriptPrelude.setup
	print script
	run scriptPrelude script []
	ScriptPrelude.cleanUp



