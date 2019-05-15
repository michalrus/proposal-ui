{-# LANGUAGE NamedFieldPuns #-}

module PromptString (spawnPromptString) where

import Brick
import Brick.Widgets.Center
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import Brick.Forms

import Types

data PromptString = PromptString
  { psTitle :: String
  , psValidator :: String -> Bool
  , psCallback :: String -> EventM Name (Either AppState Dialog)
  , psCurrentString :: String
  }

spawnPromptString :: String -> (String -> Bool) -> (String -> EventM Name (Either AppState Dialog)) -> EventM Name (Either AppState Dialog)
spawnPromptString title validator callback = do
  let
    state = PromptString title validator callback ""
  pure $ Right $ mkPromptString state

mkPromptString :: PromptString -> Dialog
mkPromptString state = Dialog { dRender = renderUI state, dHandleEvent = handleEvents state }

renderUI :: PromptString -> AppState -> [ Widget Name ]
renderUI PromptString{psCurrentString,psValidator,psTitle} astate = [ center root ]
  where
    root :: Widget Name
    root = B.borderWithLabel (str psTitle) $ vBox [ pad, padLeftRight 1 $ currentStr, pad ]
    attr = if (psValidator psCurrentString) then focusedFormInputAttr else invalidFormInputAttr
    currentStr = withDefAttr attr currentStr'
    currentStr' = str psCurrentString
    pad = str $ replicate (length psTitle) ' '

handleEvents :: PromptString -> AppState -> BrickEvent Name CustomEvent -> EventM Name (Either AppState Dialog)
handleEvents pstate@PromptString{psCallback,psCurrentString} astate event = do
  case event of
    VtyEvent (V.EvKey V.KEnter []) -> do
      -- TODO, allows invalid strings passed to callback
      psCallback psCurrentString
    VtyEvent (V.EvKey (V.KChar char) []) -> do
      pure $ Right $ mkPromptString $ pstate { psCurrentString = psCurrentString <> [char] }
    VtyEvent (V.EvKey V.KBS []) -> do
      let
        size = length psCurrentString
        newstring = take (size - 1) psCurrentString
      pure $ Right $ mkPromptString $ pstate { psCurrentString = newstring }
    other -> do
      pure $ Right $ mkPromptString $ pstate { psTitle = show other }
