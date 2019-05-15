module Types (Name(FileChoice, MainMenu, Menu1), Dialog(Dialog, dRender, dHandleEvent), AppState(AppState, asLastMsg, asDialogStack, asReplyChan), CustomEvent) where

import           Brick.BChan (BChan)
import           Brick (Widget, EventM, BrickEvent)

data Name = MainMenu | FileChoice | Menu1 | None deriving (Eq, Ord, Show)

data AppState = AppState
  { asReplyChan :: BChan ()
  , asLastMsg :: String
  , asDialogStack :: Dialog
  }

data Dialog = Dialog
  { dRender :: AppState -> [ Widget Name ]
  , dHandleEvent :: AppState -> BrickEvent Name CustomEvent -> EventM Name (Either AppState Dialog)
  }

data CustomEvent = CustomEvent  deriving Show
