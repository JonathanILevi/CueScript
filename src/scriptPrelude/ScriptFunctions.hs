module ScriptFunctions where
import qualified Prelude as P
import Prelude (($),(>>),(<$>),Functor(..),Applicative(..),Monad(..))
import qualified Control.Concurrent as B
import qualified System.IO as B

import Script

import VLCBackend

add :: Value -> Value
add (List vs) = Number $ P.foldl1 (P.+) $ fmap (\(Number v)->v) $ vs

wait :: Value -> P.IO Value
wait _ = do
	P.putStr ":"
	B.hFlush B.stdout
	
	B.hSetEcho B.stdout P.False
	String <$> P.getLine
	B.hSetEcho B.stdout P.True
	
	P.putStr "\b \b"
	B.hFlush B.stdout
	
	return Undefined
play :: Instance -> Value -> P.IO Value
play inst (String file) = do
	P.putStrLn ("Playing " P.++ file)
	VLCBackend.playFromPath inst file
	return Undefined

print :: Value -> P.IO Value
print Undefined = P.putStrLn "" >> return Undefined
print (log) = do
	P.putStrLn $ P.show log
	return Undefined
printPrefix :: Value -> P.IO Value
printPrefix (log) = do
	P.putStr $ P.show log
	B.hFlush B.stdout
	return Undefined
input :: Value -> P.IO Value
input (printLogs) = do
	printPrefix printLogs
	value <- String <$> P.getLine
	return value


