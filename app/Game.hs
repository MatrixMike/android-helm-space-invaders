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
import           FRP.Helm
import           FRP.Helm.Time
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard

import           FRP.Elerea.Param hiding (Signal)
import           FRP.Helm.Sample

--
-- Constants
--

cannonVelocity, laserVelocity, laserUpperBound :: Double
cannonVelocity  = 150
laserVelocity   = 20
laserUpperBound = 300

---

data Input =
  Input {
    inpSpace  :: Bool
  , inpArrows :: (Int,Int)
  , inpDelta :: Time
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
lyAbs cannonState = cy cannonState - 15 - ly cannonState

delta :: Signal Time
delta = fmap inSeconds (fps 35)

gameInput :: Signal Input
gameInput = sampleOn delta <|
          (Input 
           <$> Keyboard.isDown Keyboard.SpaceKey
           <*> Keyboard.arrows
           <*> delta)

data Game =
  Game {
    cannon :: CannonState
  , invaders :: [InvaderState]
  }

initialCannon :: CannonState
initialCannon = CannonState { cx = 0, cy = 200
                            , lx = 0, ly = 0
                            , laserFlying = False
                            }

initialInvaders :: [InvaderState]
initialInvaders =  concat [row1, row2, row3, row4, row5]
  where
    xposs = [-210, -180, -150, -120, -90, -60, -30, 0, 30, 60]
    row1 = createRow (-100, red)
    row2 = createRow (-70, blue)
    row3 = createRow (-40, blue)
    row4 = createRow (-10, green)
    row5 = createRow (20, green)
    createRow :: (Double, Color) -> [InvaderState]
    createRow (yy, color) =
      map (\(x, y) -> InvaderState { ix = x, iy = y, color = color
                                   , killed = False, elapsed = 0}) invaderPoss
        where
          invaderPoss = map (\x -> (x, yy)) xposs



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
        laserFlying' = ((inpSpace inp && ly state == 0) ||
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
    collisionRadius = 20

gameSignal :: Signal Game
gameSignal = foldp stepGame (Game initialCannon initialInvaders) gameInput


cannonForm :: CannonState -> Form
cannonForm state = move (cx state, cy state) $ filled red $ rect 64 32

laserForm :: CannonState -> Form
laserForm cannonState = move (lx', ly') $ filled white $ rect 2 10
  where lx' = lx cannonState
        ly' = lyAbs cannonState

invaderForm :: InvaderState -> Form
invaderForm s = move (ix s, iy s) $ filled (color s) $ rect 20 20

render :: Game -> (Int, Int) -> Element
render g (w, h) =
  centeredCollage w h $ [cannonForm (cannon g),
                         laserForm (cannon g)]
                         ++ map invaderForm liveInvaders
    where liveInvaders = filter (not . killed) (invaders g)

game :: IO ()
game = do
  run config $ render <~ gameSignal ~~ Window.dimensions
  where config = defaultConfig { windowTitle = "Space Invaders"
                               , windowIsFullscreen = False
                               , windowDimensions = (winWidth, winHeight)}
        (winWidth, winHeight) = (500, 500)

