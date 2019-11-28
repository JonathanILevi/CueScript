module MakeScript where
import qualified Prelude
import Prelude (($))

import Script

script memory =	NativeProcedure memory $
	[ Run $ Call (Ref "print") (Literal $ LitString "Ready to go...")
	, Run $ Call (Ref "printPrefix") (Literal $ LitString "Press enter to continue")
	, Run $ Ref ":"
	, Run $ Ref "print"
	, Run $ Call (Ref "play") (Literal $ LitString "doorbell-sound-effect-youtubemp3free.org.wav")
	, Assign "inp" $ Run $ Call (Ref "input") (Literal $ LitString "> ")
	, Run $ Call (Ref "print") (Ref "inp")
	, Run $ Ref ":"
	, Run $ Call (Ref "play") (Literal $ LitString "doorbell-sound-effect-youtubemp3free.org.wav")
	, Run $ Call (Ref "print") (Literal $ LitString "Ready for music!")
	, Run $ Ref ":"
	, Run $ Call (Ref "play") (Literal $ LitString "amclassical_beethoven_fur_elise.wav")
	, Run $ Ref ":"
	, Run $ Call (Ref "print") (Literal $ LitString "This is the last sound.")
	, Run $ Call (Ref "play") (Literal $ LitString "doorbell-sound-effect-youtubemp3free.org.wav")
	, Run $ Ref $ ":"
	]







