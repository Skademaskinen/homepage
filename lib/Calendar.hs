module Calendar where

import Data.Time (UTCTime)

generateEvent :: String -> UTCTime -> String
generateEvent name timestamp = name
