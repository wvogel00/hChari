module Types where

import Graphics.FreeGame.Simple (Vec2)

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