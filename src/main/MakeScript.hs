module MakeScript where
import qualified Prelude
import Prelude (($))

import Script

script =	[Run $ Call (Ref "print") (Call (Ref "add") (Literal $ LitList [Number 2, Number 4]))]







