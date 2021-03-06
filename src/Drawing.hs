module Drawing where

import Constants
import Geometry
import Interactions
import Game.Sequoia.Color
import Game.Sequoia.Text

draw_game :: Int -> V2 -> [Game Form]
draw_game fps camera =
  [ pure $ toForm $ image "assets/space.png"
  , move (-camera) . group <$> efor (entsWith eGfx) draw_gfx
  , move (-camera) . group <$> efor (entsWith eLaser) draw_lasers
  , move (-camera) . group <$> efor (entsWith eHitpoints) draw_hp
  , move (V2 32 (600 - 64)) <$> draw_minimap camera
  , move (V2 400 500) <$> draw_transmission
  , pure $ move 20 $ draw_text green LeftAligned $ show fps
  ]


draw_portrait :: String -> Form
draw_portrait portrait =
  group
    [ toForm $ image "assets/portraits/bg.png"
    , toForm $ image $ mconcat [ "assets/portraits/", portrait, ".png" ]
    , toForm $ image "assets/portraits/border.png"
    ]


draw_transmission :: Game Form
draw_transmission = fmap (fromMaybe mempty . listToMaybe) $
  efor (entsWith eSpecialThing) $ do
    Transmission person msg <- query eSpecialThing
    age <- query eAge
    pure $ draw_transmission_gfx person
         $ take (round $ characterDisplayPerSecond * age) msg


draw_transmission_gfx :: Person -> String -> Form
draw_transmission_gfx (Person name portrait) msg =
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
  pos <- interact_posIfOnScreen
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
  pos <- interact_posIfOnScreen
  hp  <- query eHitpoints
  pure . move (pos + V2 0 38)
       . scale 0.7
       . draw_text green CenterAligned
       . show @Int
       $ ceiling hp


draw_lasers :: Query Form
draw_lasers = do
  src <- interact_posIfOnScreen
  query eLaser >>= \case
    (LaserAbsPos dst, _) -> do
      pure $ traced defaultLine {lineColor = blue, lineWidth = 3} $ path [src, dst]
    (LaserRelPos rel, _) -> do
      dir <- queryDef 0 eDirection
      pure $ traced defaultLine {lineColor = blue, lineWidth = 3} $ path [src, src + rotateV2 dir rel]



debug_drawHurtboxes :: Query Form
debug_drawHurtboxes = do
  pos <- interact_posIfOnScreen
  hurts <- fmap (moveBox pos) <$> query eHurtboxes
  pure $ group $ fmap debug_drawBox hurts

debug_drawLine :: Line -> Form
debug_drawLine (Line src dst) =
  traced defaultLine {lineColor = green} $ path [src, dst]

debug_drawBox :: Box -> Form
debug_drawBox = group . fmap debug_drawLine . rectLines



minimapScale :: Num a => a
minimapScale = 32


draw_minimapBlip :: Query Form
draw_minimapBlip = do
  pos           <- query ePos
  (color, size) <- query eOnMinimap
  let size' = size * minimapScale
  pure $ move pos $ filled color $ rect size' size'


draw_minimap :: V2 -> Game Form
draw_minimap camera = do
  forms <- efor (entsWith eOnMinimap) draw_minimapBlip
  let minimap = scale (1 / minimapScale) $ move (-camera) $  group forms
  pure minimap

