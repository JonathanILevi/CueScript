module Main where

import Script (Script,evaluate)
import qualified Script
import MakeScript (script)
import ScriptPrelude
import Data.Map

main = do
	ScriptPrelude.setup
	print script
	evaluate scriptPrelude script
	ScriptPrelude.cleanUp



