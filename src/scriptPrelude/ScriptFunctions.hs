module ScriptFunctions where
import qualified Prelude as P
import Prelude (($),(>>),(<$>),Functor(..),Applicative(..),Monad(..))
import qualified Control.Concurrent as B
import qualified System.IO as B

import Script

import VLCBackend


wait _ = do
	P.putStr ":"
	B.hFlush B.stdout
	
	B.hSetEcho B.stdout P.False
	String <$> P.getLine
	B.hSetEcho B.stdout P.True
	
	P.putStr "\b \b"
	B.hFlush B.stdout
	
	return Undefined
play inst (String file:_) = do
	P.putStrLn ("Playing " P.++ file)
	VLCBackend.playFromPath inst file
	return Undefined
print (logs) = do
	P.putStrLn $ P.concatMap P.show logs
	return Undefined
printPrefix (logs) = do
	P.putStr $ P.concatMap P.show logs
	B.hFlush B.stdout
	return Undefined
input (printLogs) = do
	printPrefix printLogs
	value <- String <$> P.getLine
	return value


