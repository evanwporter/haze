{-# LANGUAGE OverloadedStrings #-}

module Main where

import Brick
import Brick.Main (defaultMain)
import Brick.Widgets.Border
import Data.Text (Text)
import Graphics.Vty (defAttr)

data Name = MainView
    deriving (Eq, Ord, Show)

data AppState = AppState
    { entries :: [Text]
    }

-- Set the rows that will be displayed
setEntries :: [Text] -> AppState -> AppState
setEntries newEntries state =
    state{entries = newEntries}

initialState :: AppState
initialState =
    AppState
        { entries = []
        }

table :: [Widget n] -> Widget n
table [] = emptyWidget
table [x] = x
table (x : xs) =
    x
        <=> hBorder
        <=> table xs

drawUI :: AppState -> [Widget Name]
drawUI state =
    [ joinBorders $
        border $
            ( table
                (map (padLeftRight 1 . txt) (entries state))
                <=> fill ' '
            )
    ]

app :: App AppState e Name
app =
    App
        { appDraw = drawUI
        , appChooseCursor = neverShowCursor
        , appHandleEvent = \_ -> return ()
        , appStartEvent = return ()
        , appAttrMap = const $ attrMap defAttr []
        }

main :: IO ()
main = do
    -- Replace this with the function from your library.
    let runtimeEntries =
            [ "Firefox"
            , "Terminal"
            , "Neovim"
            , "Discord"
            ]

    let state =
            setEntries runtimeEntries initialState

    _ <- defaultMain app state
    return ()
