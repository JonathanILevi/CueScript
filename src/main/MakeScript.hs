module MakeScript where
import qualified Prelude
import Prelude (($))

import Script

script =	[ runr "print" (callr "add" (Literal $ LitList [Number 2, Number 4]))
	, runr "print" (Literal $ LitString "Ready to go...")
	, runr "printPrefix" (Literal $ LitString "Press enter to continue")
	, runr "wait" (Literal LitUndefined)
	, runr "wait" (Literal LitUndefined)
	, Run $ Ref "doorbell"
	, Assign "inp" $ runr "input" (Literal $ LitString "> ")
	, runr "print" (Ref "inp")
	, runr "wait" (Literal LitUndefined)
	, runr "play" (Literal $ LitString "doorbell-sound-effect-youtubemp3free.org.wav")
	, runr "print" (Literal $ LitString "Ready for music!")
	, runr "wait" (Literal LitUndefined)
	, runr "play" (Literal $ LitString "amclassical_beethoven_fur_elise.wav")
	, runr "wait" (Literal LitUndefined)
	, runr "print" (Literal $ LitString "This is the last sound.")
	, runr "play" (Literal $ LitString "doorbell-sound-effect-youtubemp3free.org.wav")
	, runr "wait" (Literal LitUndefined)
	]

run a v = Run $ Call a v
runr a v = Run $ Call (Ref a) v
callr f v = Call (Ref f) v





