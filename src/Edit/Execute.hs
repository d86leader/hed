module Edit.Execute
(
) where

import Data.Map.Strict (member, insert)
import qualified Edit.Command as Command
import Edit.Command (Command(..), LinewiseMovement(..), CharacterMovement(..),
                     VSide(..), HSide(..))
import Edit.Effects (Buffer(..), Effects(..), EditAtom, newCursor)

-- |Execute editor commands to modify a buffer

runCommand :: Command -> Buffer -> EditAtom
runCommand (AddLineSelection movement) = addLineSelection movement


addLineSelection :: LinewiseMovement -> Buffer -> EditAtom
addLineSelection (AbsoluteNumber line) buffer =
    let map = cursors buffer
    in if line `member` map
       then return buffer
       else let inserted = insert line newCursor map
                updated = buffer {cursors = inserted}
            in return updated
