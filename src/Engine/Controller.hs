module Engine.Controller where

import qualified Data.Set as S


buildKeystate :: S.Set Key -> S.Set Key -> Key -> Keystate
buildKeystate old_kb kb = \k ->
  getKeystate (S.member k old_kb) $ S.member k kb


getKeystate :: Bool -> Bool -> Keystate
getKeystate False False = Up
getKeystate False True  = Press
getKeystate True  True  = Down
getKeystate True  False = Unpress

