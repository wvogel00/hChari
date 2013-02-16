{-# LANGUAGE ImplicitParams #-}

import Graphics.FreeGame.Simple
import Control.Monad
import Types

main = do
    bmp <- loadBitmapFromFile "images/HaskellLogoStyPreview-1.png"
    let ?bmp = bmp
    runSimple defaultGameParam (240,80) mainLoop

mainLoop (x,y) = do
        drawPicture $ Translate (Vec2 x y) $ BitmapPicture ?bmp -- 読み込んだビットマップを(240,80)に表示
        
        key <- askInput KeyEsc -- Escキーの状態を取得
        when key $ quitGame -- Trueならば終了
        
        key <- askInput $ KeyChar 'Z' -- Zキーの状態を取得
        return $ if key then (x+10,y) else (x,y) -- Trueのときだけ1増やす