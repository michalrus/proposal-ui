{-# LANGUAGE NamedFieldPuns #-}

module KeyToy (spawnKeyToy) where

import Brick
import qualified Graphics.Vty as V

import Types

data KeyToy = KeyToy
  { ktCallback :: EventM Name (Either AppState Dialog)
  --, ktEsk :: Maybe EncryptedSecretKey
  }

spawnKeyToy :: EventM Name (Either AppState Dialog) -> EventM Name Dialog
spawnKeyToy callback = do
  let
    state = KeyToy callback
  pure $ mkKeyToy state

mkKeyToy :: KeyToy -> Dialog
mkKeyToy state = Dialog { dRender = renderUI state, dHandleEvent = handleEvents state }

renderUI :: KeyToy -> AppState -> [ Widget Name ]
renderUI kstate astate = [ str "todo" ]

handleEvents :: KeyToy -> AppState -> BrickEvent Name CustomEvent -> EventM Name (Either AppState Dialog)
handleEvents KeyToy{ktCallback} astate event = do
  case event of
    VtyEvent (V.EvKey (V.KChar 'q') []) -> do
      ktCallback
