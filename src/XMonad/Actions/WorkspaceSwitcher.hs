module XMonad.Actions.WorkspaceSwitcher where

import XMonad as X
import XMonad.StackSet as S

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar
import Control.Monad.Reader
import Control.Monad.Trans (liftIO)
import Data.List (find, findIndex)
import Data.Maybe
import Graphics.UI.Gtk as G
import System.Glib.UTFString (glibToString)

{-getWinName :: X.Window -> X.X String-}
{-getWinName w = X.runQuery X.title w-}

{--- | Given a stack of windows, returns the titles of the windows.-}
{-getWinNamesForStack :: Maybe (S.Stack X.Window) -> X.X [String]-}
{-getWinNamesForStack Nothing = return []-}
{-getWinNamesForStack (Just s) =-}
  {-traverse getWinName $ [S.focus s] ++ (S.up s) ++ (S.down s)-}

{--- | Returns a list of (workspace name, [window names])-}
{-getWorkspaceWindows :: X.X [(String, [String])]-}
{-getWorkspaceWindows = do-}
  {-orderedWorkspaceIds <- asks (X.workspaces . config)-}
  {-allWorkspaces <- gets (S.workspaces . X.windowset)-}
  {-display <- asks display-}
  {-let-}
    {-orderedWorkspaces = catMaybes $-}
      {-[ find (\w -> S.tag w == i) allWorkspaces-}
      {-| i <- orderedWorkspaceIds-}
      {-]-}
  {-windowNames <- traverse (getWinNamesForStack . S.stack) orderedWorkspaces-}
  {-let res = zip (map S.tag orderedWorkspaces) windowNames-}
  {-X.trace (unlines $ map show res)-}
  {-return res-}

data SwitcherConfig = SwitcherConfig
  { scale    :: Int -- downscale screenshot by this much
  , cols     :: Int -- arrange thumbnails into this many columns
  }

scalePixbuf :: Int  -- scale
            -> Int  -- input width
            -> Int  -- input height
            -> Maybe Pixbuf
            -> IO (Maybe Pixbuf)
scalePixbuf _ _ _ Nothing = return Nothing
scalePixbuf scale w h (Just pb) = Just <$> pixbufScaleSimple
                                           pb
                                           (w `div` scale)
                                           (h `div` scale)
                                           InterpBilinear

getThumbnail :: DrawableClass d => 
                Int   -- origin_x
             -> Int   -- origin_y
             -> Int   -- width
             -> Int   -- height
             -> Int   -- scale
             -> d 
             -> IO (Maybe Pixbuf)
getThumbnail x y w h s win =
  pixbufGetFromDrawable win (G.Rectangle x y w h) >>=
  scalePixbuf s w h

makeIconView :: [(WorkspaceId, Pixbuf)]
             -> WorkspaceId  -- current ws
             -> Int          -- cols
             -> IO IconView
makeIconView thumbs current cols = do
  let pixbufColumn = makeColumnIdPixbuf 0
      labelColumn  = makeColumnIdString 1 :: ColumnId a String
      Just curIdx = findIndex ((== current) . fst) thumbs
  model <- listStoreNew thumbs
  customStoreSetColumn model pixbufColumn snd
  customStoreSetColumn model labelColumn fst
  iv <- iconViewNewWithModel model
  m <- iconViewGetModel iv
  iconViewSetSelectionMode iv SelectionSingle
  iconViewSetColumns iv cols
  iconViewSetReorderable iv False
  iconViewSetPixbufColumn iv pixbufColumn
  iconViewSetTextColumn iv labelColumn
  iconViewSetCursor iv (Left [curIdx] :: Either TreePath CellRenderer) False
  -- TODO: name the gtk components so they can be styled with a .gtkrc
  return iv

makeWindow :: IconView -> IO G.Window
makeWindow iv = do
  -- Can't figure out how to create a regular window and set OverrideRedirect
  -- to make the window unmanaged.  So instead, create a "popup" window which
  -- has it set.
  win <- windowNewPopup
  fixedLayout <- fixedNew
  windowSetDecorated win False
  windowSetPosition win WinPosCenter
  containerAdd win fixedLayout
  fixedPut fixedLayout iv (0, 0)
  return win

setWindowEvents :: G.Window -> IconView -> MVar (Maybe Int) -> IO ()
setWindowEvents win iv mv = do
  win `on` deleteEvent $ liftIO mainQuit >> return False
  win `on` keyReleaseEvent $ do
    keyname <- glibToString <$> eventKeyName
    liftIO $ do
      case keyname of
        "Escape" -> mainQuit >> return False
        "Return" -> do
          sel:_ <- iconViewGetSelectedItems iv
          putMVar mv (Just (head sel))
          mainQuit
          return False
        _        -> return False
  return ()

showWindow :: G.Window -> IO ()
showWindow win = do
  widgetShowAll win
  widgetGrabFocus win
  Just dw <- widgetGetWindow win
  gs <- keyboardGrab dw True G.currentTime
  mainGUI
  return ()

getWorkspaceImage :: Int -> WorkspaceId -> X Pixbuf
getWorkspaceImage scale wid = do
  windows $ S.view wid
  X.io $ do
    rw <- drawWindowGetDefaultRootWindow
    (w, h) <- drawableGetSize rw
    Just pb_thumb <- getThumbnail 0 0 w h scale rw
    return pb_thumb

switcherMain :: SwitcherConfig
             -> [WorkspaceId]
             -> WorkspaceId
             -> [Pixbuf]
             -> MVar (Maybe Int)
             -> IO ()
switcherMain sconfig workspaceIds currentWs thumbnails mv = do
  iv <- makeIconView (zip workspaceIds thumbnails) currentWs (cols sconfig)
  win <- makeWindow iv
  setWindowEvents win iv mv
  showWindow win
  return ()

runSwitcher :: SwitcherConfig -> X ()
runSwitcher sconfig = do
  mv <- liftIO newEmptyMVar
  workspaceIds <- asks (X.workspaces . config)
  currentWs <- X.gets (S.currentTag . windowset)
  thumbnails <- traverse (getWorkspaceImage (scale sconfig)) workspaceIds
  windows $ S.view currentWs
  X.io $ forkIO $ switcherMain sconfig workspaceIds currentWs thumbnails mv
  response <- X.io $ takeMVar mv
  case response of
    Nothing -> return ()
    Just wsIdx -> windows $ S.view (workspaceIds !! wsIdx)
  return ()
