module MakeScript where
import qualified Prelude
import Prelude (($))

import Script

script =	[ Call "print" [Value $ String "Ready to go..."]
	, Call "printPrefix" [Value $ String "Press enter to continue"]
	, Call ":" []
	, Call "print" []
	, Call "play" [Value $ String "doorbell-sound-effect-youtubemp3free.org.wav"]
	, Call "print" [Call "input" [Value $ String "> "]]
	, Call ":" []
	, Call "play" [Value $ String "doorbell-sound-effect-youtubemp3free.org.wav"]
	, Call "print" [Value $ String "Ready for music!"]
	, Call ":" []
	, Call "play" [Value $ String "amclassical_beethoven_fur_elise.wav"]
	, Call ":" []
	, Call "print" [Value $ String "This is the last sound."]
	, Call "play" [Value $ String "doorbell-sound-effect-youtubemp3free.org.wav"]
	, Call ":" []
	]







