{-# LANGUAGE NamedFieldPuns #-}

module KeyToy (spawnKeyToy) where

import Brick
import qualified Graphics.Vty as V

import Types

data KeyToy = KeyToy
  { ktCallback :: EventM Name DialogReply
  --, ktEsk :: Maybe EncryptedSecretKey
  }

spawnKeyToy :: EventM Name DialogReply -> EventM Name Dialog
spawnKeyToy callback = do
  let
    state = KeyToy callback
  pure $ mkKeyToy state

mkKeyToy :: KeyToy -> Dialog
mkKeyToy state = Dialog { dRender = renderUI state, dHandleEvent = handleEvents state }

renderUI :: KeyToy -> AppState -> [ Widget Name ]
renderUI _kstate _astate = [ str "todo" ]

handleEvents :: KeyToy -> AppState -> BrickEvent Name CustomEvent -> EventM Name DialogReply
handleEvents kstate@KeyToy{ktCallback} _astate event = do
  case event of
    VtyEvent (V.EvKey (V.KChar 'q') []) -> do
      ktCallback
    _ -> pure $ DialogReplyContinue $ mkKeyToy kstate
