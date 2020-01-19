module Drawing (drawGame) where

import Types
import Geometry
import Game.Sequoia.Color

drawGame :: [Query Form]
drawGame =
  [ draw_gfx
  , draw_lasers

  , debug_drawHurtboxes
  ]

draw_gfx :: Query Form
draw_gfx = join $ query eGfx


draw_lasers :: Query Form
draw_lasers = do
  src <- query ePos
  query eLaser >>= \case
    (LaserAbsPos dst, _) -> do
      pure $ traced defaultLine {lineColor = blue, lineWidth = 3} $ path [src, dst]
    (LaserRelPos rel, _) -> do
      dir <- queryDef 0 eDirection
      pure $ traced defaultLine {lineColor = blue, lineWidth = 3} $ path [src, src + rotateV2 dir rel]



debug_drawHurtboxes :: Query Form
debug_drawHurtboxes = do
  pos <- query ePos
  hurts <- fmap (moveBox pos) <$> query eHurtboxes
  pure $ group $ fmap debug_drawBox hurts

debug_drawLine :: Line -> Form
debug_drawLine (Line src dst) =
  traced defaultLine {lineColor = green} $ path [src, dst]

debug_drawBox :: Box -> Form
debug_drawBox = group . fmap debug_drawLine . rectLines

