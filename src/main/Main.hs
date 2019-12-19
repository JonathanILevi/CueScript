module Main where

import Script (start, Breakpoint(..))
import qualified Script
import MakeScript (script)
import ScriptPrelude
import Data.Map
import Control.Monad.State
import Control.Monad.Reader

main = do
	prelude <- createScriptPrelude
	executeBreakpoint $ runReader (start script) (prelude)
	>>= maybe (pure Nothing) executeBreakpoint


executeBreakpoint (Breakpoint (Just (action, next))) = do
	rd <- runAction action
	return $ Just (next rd)
executeBreakpoint (Breakpoint Nothing) = print "done" >> pure Nothing

