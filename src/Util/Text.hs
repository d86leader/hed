module Util.Text
( split2
) where


import Data.Text (Text)
import qualified Data.Text as Text


-- |Split into three chunks by two indicies: [0; l), [l, r], (r, +inf]
split2 :: (Int, Int) -> Text -> (Text, Text, Text)
split2 (l, r) text =
    let len = Text.length text
        roff = len - r - 1 -- right offset, how l is left offset
        left  = Text.take l text
        right = Text.takeEnd roff text
        mid   = Text.drop l . Text.dropEnd roff $ text
    in (left, mid, right)
