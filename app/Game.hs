--
-- Original author: Alex Greif
--
-- This code heavily based on code here:
-- https://github.com/agreif/hs-helm-samples/tree/master/space_invaders
--
-- Updated for latest version of Helm by Sean Seefried
--


module Game where

import           Control.Arrow
import           Control.Applicative
import           FRP.Helm
import           FRP.Helm.Time
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard
import qualified FRP.Helm.Touch as Touch

import           FRP.Elerea.Param hiding (Signal)
import           FRP.Helm.Sample

--
-- Constants
--

--
-- World co-ordinates are a box 100x100 pixels
--

worldWidth :: Double
worldWidth = 100
cannonVelocity, laserVelocity, laserUpperBound :: Double
cannonVelocity  = 50
laserVelocity   = 4
laserUpperBound = 60



---

data Input =
  Input {
    inpSpace      :: Bool
  , inpTouches    :: [(Float, Float)]
  , inpArrows     :: (Int,Int)
  , inpDelta      :: Time
  } deriving (Read, Show, Eq)

data CannonState = CannonState { cx :: Double, cy :: Double
                               , lx :: Double, ly :: Double
                               , laserFlying :: Bool
                               } deriving (Read, Show, Eq)

data InvaderState = InvaderState { ix :: Double
                                 , iy :: Double
                                 , color :: Color
                                 , killed :: Bool
                                 , elapsed :: Time
                                 } deriving (Read, Show, Eq)

lyAbs :: CannonState -> Double
lyAbs cannonState = cy cannonState - 3 - ly cannonState

delta :: Signal Time
delta = fmap inSeconds (fps 35)

gameInput :: Signal Input
gameInput = sampleOn delta <|
          (Input 
           <$> Keyboard.isDown Keyboard.SpaceKey
           <*> Touch.positions
           <*> Keyboard.arrows
           <*> delta)

data Game =
  Game {
    cannon :: CannonState
  , invaders :: [InvaderState]
  }

initialCannon :: CannonState
initialCannon = CannonState { cx = 0, cy = 40
                            , lx = 0, ly = 0
                            , laserFlying = False
                            }

initialInvaders :: [InvaderState]
initialInvaders =  concat [row1, row2, row3, row4, row5]
  where
    xposs = [-42, -36, -30, -24, -18, -12, -6, 0, 6, 12]
    row1 = createRow (-20, red)
    row2 = createRow (-14, blue)
    row3 = createRow (-8, blue)
    row4 = createRow (-2, green)
    row5 = createRow (4, green)
    createRow :: (Double, Color) -> [InvaderState]
    createRow (yy, color) =
      map (\(x, y) -> InvaderState { ix = x, iy = y, color = color
                                   , killed = False, elapsed = 0}) invaderPoss
        where
          invaderPoss = map (\x -> (x, yy)) xposs

touchDown :: Input -> Bool
touchDown g = inpSpace g || (not . null $ inpTouches g)

stepGame :: Input -> Game -> Game
stepGame inp g = g { cannon = cannon', invaders = invaders' }
  where
    cannon' = stepCannon anyKilledByCannon (cannon g)
    (invaders', anyKilledByCannon) = stepInvaders (invaders g)

    stepCannon :: Bool -> CannonState -> CannonState
    stepCannon anyKilledByCannon state =
      state { cx = cx', lx = lx', ly = ly', laserFlying = laserFlying' }
      where
        (dx,_) = inpArrows inp
        cx' = cx state + (fromIntegral dx * cannonVelocity * inpDelta inp)
        laserFlying' = ((touchDown inp && ly state == 0) ||
                       (ly state /= 0 && ly state < laserUpperBound)) && (not anyKilledByCannon)
        lx' | laserFlying' = lx state
            | otherwise = cx'
        ly' = if laserFlying' then ly state + laserVelocity else 0

    stepInvaders :: [InvaderState] -> ([InvaderState],Bool)
    stepInvaders ss = id *** any id $ unzip (map stepInvader ss)
    stepInvader :: InvaderState -> (InvaderState, Bool)
    stepInvader s = (invader', killedByCannon)
      where
        invader' =
          s { ix = ix'
            , iy = iy'
            , killed = killed'
            , elapsed = elapsed'
            }
        elapsed' = elapsed s + inpDelta inp
        ix' = ix s
        iy' = iy s
        killedByCannon = not (killed s) && laserFlying (cannon g)
                               && collidesWith (lx (cannon g), lyAbs (cannon g)) (ix', iy')
        killed' = killed s || killedByCannon

collidesWith :: (Double, Double) -> (Double, Double) -> Bool
collidesWith (x,y) (x',y') = (x'-x)^2 + (y-y')^2 < collisionRadius^2
  where
    collisionRadius = 4

gameSignal :: Signal Game
gameSignal = foldp stepGame (Game initialCannon initialInvaders) gameInput


cannonForm :: Double -> CannonState -> Form
cannonForm scale state = move (scale*cx state, scale*cy state) $
                           filled red $ rect (scale*10) (scale*5)

laserForm :: Double -> CannonState -> Form
laserForm scale cannonState = move (scale*lx', scale*ly') $
                                filled white $ rect (scale*0.4) (scale*2)
  where lx' = lx cannonState
        ly' = lyAbs cannonState

invaderForm :: Double -> InvaderState -> Form
invaderForm scale s = move (scale*(ix s), scale*(iy s)) $ filled (color s) $ rect (scale*4) (scale*4)

render :: Game -> (Int, Int) -> Element
render g (w, h) =
  centeredCollage w h $ [cannonForm scale (cannon g),
                         laserForm scale (cannon g)]
                         ++ map (invaderForm scale) liveInvaders
    where
      liveInvaders = filter (not . killed) (invaders g)
      scale = fromIntegral (min w h ) / worldWidth

game :: Bool -> IO ()
game isMobile = do
  run config $ render <~ gameSignal ~~ Window.dimensions
  where
    config =
      case isMobile of
        True -> defaultConfig { windowIsFullscreen = True }
        False -> 
          defaultConfig { windowTitle = "Space Invaders"
                                 , windowIsFullscreen = False
                                 , windowDimensions = (winWidth, winHeight)}
    (winWidth, winHeight) = (500, 500)

