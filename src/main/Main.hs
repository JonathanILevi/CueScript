module Main where

import Script (Script,run)
import qualified Script
import MakeScript (script)
import ScriptPrelude
import Data.Map

main = do
	scriptPrelude <- createScriptPrelude
	print script
	run scriptPrelude script []


