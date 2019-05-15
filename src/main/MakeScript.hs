module MakeScript where
import qualified Prelude
import Prelude (($))

import Script

script =	[ Call ":" []
	, Call "play" [Value $ String "/home/jonathan/Downloads/doorbell-sound-effect-youtubemp3free.org.wav"]
	, Call ":" []
	, Call "play" [Value $ String "/home/jonathan/Downloads/doorbell-sound-effect-youtubemp3free.org.wav"]
	, Call ":" []
	, Call "play" [Value $ String "/home/jonathan/Downloads/amclassical_beethoven_fur_elise.wav"]
	, Call ":" []
	, Call "play" [Value $ String "/home/jonathan/Downloads/doorbell-sound-effect-youtubemp3free.org.wav"]
	, Call ":" []
	]







