{-# LANGUAGE ImplicitParams #-}

import Graphics.FreeGame.Simple
import Control.Monad
import Types

main = do
    bmp <- loadBitmapFromFile "images/HaskellLogoStyPreview-1.png"
    let ?bmp = bmp
    runSimple defaultGameParam (240,80) mainLoop

mainLoop (x,y) = do
        drawPicture $ Translate (Vec2 x y) $ BitmapPicture ?bmp -- �ǂݍ��񂾃r�b�g�}�b�v��(240,80)�ɕ\��
        
        key <- askInput KeyEsc -- Esc�L�[�̏�Ԃ��擾
        when key $ quitGame -- True�Ȃ�ΏI��
        
        key <- askInput $ KeyChar 'Z' -- Z�L�[�̏�Ԃ��擾
        return $ if key then (x+10,y) else (x,y) -- True�̂Ƃ�����1���₷