module Types where

import Graphics.FreeGame.Base

type Field = [Int]

(winW,winH) = (600,400)

data Player = Player{
    name :: String,
    onJump :: Bool,
    pos :: Vec2
    }

data GameInfo = GameInfo{
    field :: Field,
    player :: Player,
    rival :: Maybe Player,
    record :: Int,
    announce :: String
    }

settingFile = "setting.chr"

hChariGameParam = GameParam 60 (winW,winH) "hChari" True True (Color 255 255 255 0) (Vec2 0 0)