module MakeScript where
import qualified Prelude
import Prelude (($))

import Script

script memory =	NativeProcedure memory $
	[ Run $ Call (Ref "print") (Literal $ String "Ready to go...")
	, Run $ Call (Ref "printPrefix") (Literal $ String "Press enter to continue")
	, Run $ Ref ":"
	, Run $ Ref "print"
	, Run $ Call (Ref "play") (Literal $ String "doorbell-sound-effect-youtubemp3free.org.wav")
	, Assign "inp" $ Run $ Call (Ref "input") (Literal $ String "> ")
	, Run $ Call (Ref "print") (Ref "inp")
	, Run $ Ref ":"
	, Run $ Call (Ref "play") (Literal $ String "doorbell-sound-effect-youtubemp3free.org.wav")
	, Run $ Call (Ref "print") (Literal $ String "Ready for music!")
	, Run $ Ref ":"
	, Run $ Call (Ref "play") (Literal $ String "amclassical_beethoven_fur_elise.wav")
	, Run $ Ref ":"
	, Run $ Call (Ref "print") (Literal $ String "This is the last sound.")
	, Run $ Call (Ref "play") (Literal $ String "doorbell-sound-effect-youtubemp3free.org.wav")
	, Run $ Ref $ ":"
	]







