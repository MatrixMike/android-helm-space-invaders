--
-- Original author: Alex Greif
--
-- This code heavily based on code here:
-- https://github.com/agreif/hs-helm-samples/tree/master/space_invaders
--
-- Updated for latest version of Helm by Sean Seefried
--


module Game where

import           FRP.Helm
import           FRP.Helm.Time
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Keyboard as Keyboard

import           FRP.Elerea.Param hiding (Signal)
import           FRP.Helm.Sample

import Debug.Trace

--
-- Constants
--

cannonVelocity :: Double
cannonVelocity = 100

laserVelocity :: Double
laserVelocity = 20

laserUpperBound :: Double
laserUpperBound = 300

---

data Input =
  Input {
    inpSpace  :: Bool
  , inpArrows :: (Int,Int)
  , inpDelta :: Time
  }

data CannonState = CannonState { cx :: Double, cy :: Double
                               , lx :: Double, ly :: Double
                               , laserFlying :: Bool}

data InvaderState = InvaderState { ix :: Double
                                 , iy :: Double
                                 , color :: Color
                                 , killed :: Bool
                                 , elapsed :: Time
                                 }

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

cannonSignal :: Signal CannonState
cannonSignal = signal
  where
    initialState = CannonState { cx = 0, cy = 200
                               , lx = 0, ly = 0
                               , laserFlying = False}
    signal = foldp newState initialState gameInput
    newState :: Input -> CannonState -> CannonState
    newState inp state = state { cx = cx', lx = lx', ly = ly', laserFlying = laserFlying' }
      where
        (dx,_) = inpArrows inp
        cx' = cx state + (fromIntegral dx * cannonVelocity * inpDelta inp)
        laserFlying' = (inpSpace inp && ly state == 0) || (ly state /= 0 && ly state < laserUpperBound)
        lx' | laserFlying' = lx state
            | otherwise = cx'
        ly' = if laserFlying' then ly state + laserVelocity else 0

invaderSignal :: Signal [InvaderState]
invaderSignal = foldp newStates initialStates combined
  where
    initialStates = concat [row1, row2, row3, row4, row5]
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
    combined = (,) <$> gameInput <*> cannonSignal
    newStates :: (Input, CannonState) -> [InvaderState] -> [InvaderState]
    newStates = map . newState
    newState :: (Input, CannonState) -> InvaderState -> InvaderState
    newState (inp, cannonState) state =
      state { ix = ix'
            , iy = iy'
            , killed = killed'
            , elapsed = elapsed'
            }
      where
        elapsed' = elapsed state + inpDelta inp
        ix' = ix state
        iy' = iy state
        killed' = killed state
                    || (laserFlying cannonState
                        && sqrt ((lx cannonState - ix')^2 + (lyAbs cannonState - iy')^2) < 20)

cannonForm :: CannonState -> Form
cannonForm state = move (cx state, cy state) $ filled red $ rect 64 32

laserForm :: CannonState -> Form
laserForm cannonState = move (lx', ly') $ filled white $ rect 2 10
  where lx' = lx cannonState
        ly' = lyAbs cannonState

invaderForm :: InvaderState -> Form
invaderForm s = move (ix s, iy s) $ filled (color s) $ rect 20 20

render :: CannonState -> [InvaderState] -> (Int, Int) -> Element
render cannonState invaderStates (w, h) =
  centeredCollage w h $ [cannonForm cannonState,
                         laserForm cannonState]
                         ++ map invaderForm liveInvaders
    where liveInvaders = filter (not . killed) invaderStates

game :: IO ()
game = do
  run config $ render <~ cannonSignal ~~ invaderSignal ~~ Window.dimensions
  where config = defaultConfig { windowTitle = "Space Invaders"
                               , windowIsFullscreen = False
                               , windowDimensions = (winWidth, winHeight)}
        (winWidth, winHeight) = (500, 500)

