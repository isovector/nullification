module Drawing (drawGame, draw_text) where

import Geometry
import Interactions
import Game.Sequoia.Color
import Game.Sequoia.Text

drawGame :: [Query Form]
drawGame =
  [ draw_gfx
  , draw_lasers

  -- , debug_drawHurtboxes

  , draw_hp
  ]

draw_gfx :: Query Form
draw_gfx = do
  pos <- interact_onlyIfOnScreen
  origin <- queryDef 0 eOrigin
  Radians dir <- queryDef (Radians 0) eDirection
  fmap (move pos . rotate dir . move (- origin)) $ join $ query eGfx


draw_text :: String -> Form
draw_text
  = toForm
  . text
  . color green
  . monospace
  . stringText

draw_hp :: Query Form
draw_hp = do
  pos <- interact_onlyIfOnScreen
  hp  <- query eHitpoints
  pure . move (pos + V2 0 38)
       . scale 0.7
       . draw_text
       . show @Int
       $ ceiling hp


draw_lasers :: Query Form
draw_lasers = do
  src <- interact_onlyIfOnScreen
  query eLaser >>= \case
    (LaserAbsPos dst, _) -> do
      pure $ traced defaultLine {lineColor = blue, lineWidth = 3} $ path [src, dst]
    (LaserRelPos rel, _) -> do
      dir <- queryDef 0 eDirection
      pure $ traced defaultLine {lineColor = blue, lineWidth = 3} $ path [src, src + rotateV2 dir rel]



debug_drawHurtboxes :: Query Form
debug_drawHurtboxes = do
  pos <- interact_onlyIfOnScreen
  hurts <- fmap (moveBox pos) <$> query eHurtboxes
  pure $ group $ fmap debug_drawBox hurts

debug_drawLine :: Line -> Form
debug_drawLine (Line src dst) =
  traced defaultLine {lineColor = green} $ path [src, dst]

debug_drawBox :: Box -> Form
debug_drawBox = group . fmap debug_drawLine . rectLines

