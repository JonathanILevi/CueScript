module Main where

import Script (eval)
import qualified Script
import MakeScript (script)
import ScriptPrelude
import Data.Map
import Control.Monad.State
import Control.Monad.Reader

main = do
	prelude <- createScriptPrelude
	print $ runReader (eval script) (prelude)


