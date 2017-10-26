module XMonad.Actions.WorkspaceSwitcher where

import XMonad as X
import XMonad.StackSet as S

import Control.Monad.Reader
import Control.Monad.Trans (liftIO)
import Data.List (find)
import Data.Maybe
import Graphics.UI.Gtk as G
import System.Glib.UTFString (glibToString)

getWinName :: X.Window -> X.X String
getWinName w = X.runQuery X.title w

-- | Given a stack of windows, returns the titles of the windows.
getWinNamesForStack :: Maybe (S.Stack X.Window) -> X.X [String]
getWinNamesForStack Nothing = return []
getWinNamesForStack (Just s) =
  traverse getWinName $ [S.focus s] ++ (S.up s) ++ (S.down s)

-- | Returns a list of (workspace name, [window names])
getWorkspaceWindows :: X.X [(String, [String])]
getWorkspaceWindows = do
  orderedWorkspaceIds <- asks (X.workspaces . config)
  allWorkspaces <- gets (S.workspaces . X.windowset)
  display <- asks display
  let
    orderedWorkspaces = catMaybes $
      [ find (\w -> S.tag w == i) allWorkspaces
      | i <- orderedWorkspaceIds
      ]
  windowNames <- traverse (getWinNamesForStack . S.stack) orderedWorkspaces
  let res = zip (map S.tag orderedWorkspaces) windowNames
  X.trace (unlines $ map show res)
  return res

data SwitcherXState = SwitcherXState
  { xWorkspaces    :: [WorkspaceId]
  , xCurrentWs     :: WorkspaceId
  }

data SwitcherConfig = SwitcherConfig
  { scale    :: Int -- downscale screenshot by this much
  , cols     :: Int -- arrange thumbnails into this many columns
  , xstate   :: Maybe SwitcherXState
  }

type SwitcherM = ReaderT SwitcherConfig IO

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
             -> d 
             -> SwitcherM (Maybe Pixbuf)
getThumbnail x y w h win = do
  s <- asks scale
  liftIO $ do
    pb <- pixbufGetFromDrawable win (G.Rectangle x y w h)
    scalePixbuf s w h pb

makeThumbnailGrid :: [Pixbuf]
                  -> Int  -- thumb w
                  -> SwitcherM IconView
makeThumbnailGrid pbs tw = do
  let pixbufColumn = makeColumnIdPixbuf 0
      labelColumn  = makeColumnIdString 1 :: ColumnId a String
  config <- ask
  liftIO $ do
    model <- listStoreNew ([0..8] :: [Int])
    customStoreSetColumn model pixbufColumn (\r -> pbs !! r)
    customStoreSetColumn model labelColumn (\r -> show r)

    iv <- iconViewNewWithModel model
    m <- iconViewGetModel iv
    iconViewSetSelectionMode iv SelectionSingle
    iconViewSetColumns iv (cols config)
    iconViewSetReorderable iv False
    iconViewSetPixbufColumn iv pixbufColumn
    iconViewSetTextColumn iv labelColumn
    iconViewSetCursor iv (Left [3] :: Either TreePath CellRenderer) False

    -- TODO: name the gtk components so they can be styled with a .gtkrc
    return iv

-- get workspace ids
-- get pixbuf of each workspace
-- build model
-- show switcher
-- change current workspace based on selection

switcherMain :: SwitcherM (Maybe WorkspaceId)
switcherMain = do
  config <- ask
  rw <- liftIO drawWindowGetDefaultRootWindow
  (w, h) <- liftIO $ drawableGetSize rw
  pb_thumb <- getThumbnail 0 0 w h rw

  let thumb_w = w `div` (scale config)
      thumb_h = h `div` (scale config)

  -- Can't figure out how to create a regular window and set OverrideRedirect
  -- to make the window unmanaged.  So instead, create a "popup" window which
  -- has it set.
  win <- liftIO windowNewPopup

  iv <- makeThumbnailGrid (catMaybes $ take 9 $ repeat pb_thumb) thumb_w
  fixedLayout <- liftIO fixedNew
  liftIO $ do
    windowSetDecorated win False
    windowSetPosition win WinPosCenter
    win `on` deleteEvent $ liftIO mainQuit >> return False
    win `on` keyReleaseEvent $ do
      keyname <- glibToString <$> eventKeyName
      liftIO $ do
        putStrLn $ "keyrelease: " ++ keyname
        case keyname of
          "Escape" -> mainQuit >> return False
          "Return" -> do
            sel <- iconViewGetSelectedItems iv
            putStrLn $ "selected: " ++ (show sel)
            return False
          _        -> return False
    containerAdd win fixedLayout
    fixedPut fixedLayout iv (0, 0)

  liftIO $ do
    widgetShowAll win
    widgetGrabFocus win
    Just dw <- widgetGetWindow win
    gs <- keyboardGrab dw True G.currentTime
    putStrLn $ "grabStatus: " ++ (show gs)
    mainGUI
    -- TODO block until exit, then get selection and return it?
    return Nothing

runSwitcher :: SwitcherConfig -> X ()
runSwitcher sconfig = do
  workspaceIds <- asks (X.workspaces . config)
  currentWs <- gets $ S.currentTag . windowset
  let x = SwitcherXState { xWorkspaces = workspaceIds
                         , xCurrentWs = currentWs
                         }
      sconfig' = sconfig { xstate = Just x }
  selectedWorkspaceId <- X.io $ runReaderT switcherMain sconfig'
  return ()
