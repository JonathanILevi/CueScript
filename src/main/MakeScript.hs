module MakeScript where
import qualified Prelude
import Prelude (($))

import Script

script =	[ Call "print" [Literal $ String "Ready to go..."]
	, Call "printPrefix" [Literal $ String "Press enter to continue"]
	, Call ":" []
	, Call "print" []
	, Call "play" [Literal $ String "doorbell-sound-effect-youtubemp3free.org.wav"]
	, Call "print" [Call "input" [Literal $ String "> "]]
	, Call ":" []
	, Call "play" [Literal $ String "doorbell-sound-effect-youtubemp3free.org.wav"]
	, Call "print" [Literal $ String "Ready for music!"]
	, Call ":" []
	, Call "play" [Literal $ String "amclassical_beethoven_fur_elise.wav"]
	, Call ":" []
	, Call "print" [Literal $ String "This is the last sound."]
	, Call "play" [Literal $ String "doorbell-sound-effect-youtubemp3free.org.wav"]
	, Call ":" []
	]







