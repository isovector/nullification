module Drawing (drawGame, draw_text, draw_portrait, draw_transmission) where

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


draw_portrait :: String -> Form
draw_portrait portrait =
  group
    [ toForm $ image "assets/portraits/bg.png"
    , toForm $ image $ mconcat [ "assets/portraits/", portrait, ".png" ]
    , toForm $ image "assets/portraits/border.png"
    ]

draw_transmission :: String -> String -> String -> Form
draw_transmission portrait name msg =
  move (V2 ((/2) $ -width-100) (-height / 2)) $
  group
    [ draw_portrait portrait
    , move (V2 (100 + width / 2) (height / 2)) $ filled (rgba 0 0 0.2 0.5) $ rect 400 100
    , move (V2 105 (-10)) $ draw_text yellow LeftAligned name
    , move (V2 120 15) $ scale 0.7 $ draw_text white LeftAligned msg
    ]
  where
    width = 400
    height = 100

draw_gfx :: Query Form
draw_gfx = do
  pos <- interact_onlyIfOnScreen
  origin <- queryDef 0 eOrigin
  Radians dir <- queryDef (Radians 0) eDirection
  fmap (move pos . rotate dir . move (- origin)) $ join $ query eGfx


draw_text :: Color -> Alignment -> String -> Form
draw_text c a
  = toForm
  . text
  . topVAligned
  . (\x -> x { textAlignment = a } )
  . color c
  . monospace
  . stringText

draw_hp :: Query Form
draw_hp = do
  pos <- interact_onlyIfOnScreen
  hp  <- query eHitpoints
  pure . move (pos + V2 0 38)
       . scale 0.7
       . draw_text green CenterAligned
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

