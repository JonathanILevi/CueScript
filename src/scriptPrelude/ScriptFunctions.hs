module ScriptFunctions where
import qualified Prelude as P
import Prelude (($),(>>),(<$>),Functor(..),Applicative(..),Monad(..))

import Script

import VLCBackend

wait _ = do
	P.print("!:!")
	String <$> P.getLine
play (String file:_) = do
	P.print("!play!")
	VLCBackend.play (file)
	return Undefined


