{-# LANGUAGE ImplicitParams #-}

import Graphics.FreeGame.Simple
import Control.Monad

main = do
    bmp <- loadBitmapFromFile "HaskellLogoStyPreview-1.png"
    let ?bmp = bmp
    runSimple defaultGameParam (240,80) mainLoop

mainLoop (x,y) = do
        drawPicture $ Translate (Vec2 x y) $ BitmapPicture ?bmp -- �ǂݍ��񂾃r�b�g�}�b�v��(240,80)�ɕ\��
        
        key <- askInput KeyEsc -- Esc�L�[�̏�Ԃ��擾
        when key $ quitGame -- True�Ȃ�ΏI��
        
        key <- askInput $ KeyChar 'Z' -- Z�L�[�̏�Ԃ��擾
        return $ if key then (x+10,y) else (x,y) -- True�̂Ƃ�����1���₷
{-
{-# LANGUAGE TemplateHaskell #-}
import Graphics.FreeGame
import Data.Word
import Data.Array.Repa
import Control.Monad
import Control.Lens

data World = World{
  _object :: Picture,
  _pos :: (Float, Float)
  }

$(makeLenses ''World)

initWorld :: Picture ->  World
initWorld p = World { _object = p, _pos = (100,300) }

renderCircle :: Int -> (Word8, Word8, Word8, Word8) -> Bitmap
renderCircle size (r,g,b,a) = Bitmap $ fromFunction (Z :. size :. size :. 4) render where
    center = fromIntegral size / 2
    render (Z:.y:.x:.0)
        | s < 0 = a
        | s >= 1 = 0
        | otherwise = floor ((1 - s) * 256)
        where
            r = sqrt $ (fromIntegral y - center) ^ 2 + (fromIntegral x - center) ^ 2
            s = r - fromIntegral size / 2
    render (Z:._:._:.c) = [undefined,b,g,r] !! c

draw :: World -> Game ()
draw w = let (x,y) = (w ^. pos) in drawPicture . Translate (Vec2 x y) $ (w ^. object)

main :: IO (Maybe ())
main = runGame defaultGameParam $ do
  p <- loadPicture $ renderCircle 128 (128, 216, 128, 255)
  run $ initWorld p

  where
    run :: World -> Game ()
    run w = do
      isQuit <- askInput KeyEsc

      draw w

      tick
      unless isQuit $ run w
      -}