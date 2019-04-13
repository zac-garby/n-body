{-# LANGUAGE OverloadedStrings #-}

import Linear.V2
import Linear.V3
import Linear.V4
import Linear.Metric
import Control.Monad
import Data.Word8 (Word8)
import Data.List (sort)
import SDL hiding (Vector, Point, transpose)
import SDL.Primitive (Pos, fillCircle)

g = 6.67408e-11
au = 1.49598e11
dt = 1000

-- +ve z-ordinate means "into" the screen
type Vector = V3 Double
type Universe = [Body]
type Point = (Int, Int)
type Size = (Int, Int)
type Colour = V4 Word8

data Viewport = Viewport Double Double Double Double

data Body = Body
    { pos :: Vector
    , vel :: Vector
    , acc :: Vector
    , mass :: Double
    , colour :: Colour }
    deriving (Show, Eq)

instance Ord Body where
    Body{pos=(V3 _ _ z)} `compare` Body{pos=(V3 _ _ z')} = z' `compare` z

update :: Double -> Universe -> Body -> Body
update dt bodies body@Body{pos=pos, vel=vel, acc=acc, mass=mass, colour=colour}
    = Body pos' vel' acc' mass colour
    where pos' = pos + ((dt *) <$> vel)
          vel' = vel + ((dt *) <$> acc)
          acc' = (/ mass) <$> forceAccum bodies body

stepUniverse :: Double -> Universe -> Universe
stepUniverse dt universe = map (update dt universe) universe

force :: Body -> Body -> Vector
force Body{pos=pos, mass=mass} Body{pos=pos', mass=mass'}
    = (magnitude *) <$> (normalize $ pos' - pos)
    where dist = distance pos pos'
          magnitude = (g * mass * mass') / (dist * dist)

forceAccum :: Universe -> Body -> Vector
forceAccum bodies self
    = sum [ force self other | other <- bodies, other /= self ]

transpose :: Viewport -> Size -> Vector -> Point
transpose (Viewport x y w h) (ox, oy) (V3 i j _) = (i', j')
    where i' = round $ (i - x) * (fromIntegral ox / w)
          j' = round $ (j - y) * (fromIntegral oy / h)

point2pos :: (Integral a) => (a, a) -> Pos
point2pos (x, y) = V2 (fromIntegral x) (fromIntegral y)

main :: IO ()
main = do
    let size = (512, 512)
        viewport = Viewport (-2 * au) (-2 * au) (4 * au) (4 * au)
        universe =
            [ Body (V3 0 0 0) (V3 0 0 0) (pure 0) 1.989e+30 (V4 255 255 0 255)
            , Body (V3 au 0 0) (V3 0 30000 10000) (pure 0) 5.972e+26 (V4 255 255 255 255)
            , Body (V3 (au * 1.2) 0 0) (V3 0 (-20000) 0) (pure 0) 3.531e+23 (V4 255 128 255 255)
            , Body (V3 (au * 0.8) (au * 0.8) 0) (V3 0 0 30000) (pure 0) 5.972e+24 (V4 255 0 0 255) ]

    initializeAll
    window <- createWindow "n-body" defaultWindow
    windowSize window $= point2pos size
    renderer <- createRenderer window (-1) defaultRenderer
    gameLoop viewport size renderer universe

gameLoop :: Viewport -> Size -> Renderer -> Universe -> IO ()
gameLoop viewport size renderer universe = do
    events <- pollEvents
    
    let shouldQuit = any (\x -> eventPayload x == QuitEvent) events
    
    rendererDrawColor renderer $= V4 0 0 0 255
    clear renderer
    drawUniverse viewport size renderer universe
    present renderer
    
    unless shouldQuit $ gameLoop viewport size renderer (stepUniverse dt universe)

drawBody :: Viewport -> Size -> Renderer -> Body -> IO ()
drawBody viewport size renderer Body{pos=pos, colour=colour} = fillCircle renderer screenPos 3 colour
    where screenPos = point2pos $ transpose viewport size pos

drawUniverse :: Viewport -> Size -> Renderer -> Universe -> IO ()
drawUniverse viewport size renderer universe = mapM_ (drawBody viewport size renderer) (sort universe)
