--
-- xmonad.hs - xmonad config file
--

{-# LANGUAGE DeriveDataTypeable #-}

import Control.Monad

import System.Process
import System.Exit

import Data.List
import Data.Ord
import qualified Data.Foldable as Fold
import qualified Data.Sequence as Seq
import qualified Data.Map      as M
import Data.Maybe
import Data.Time
import Data.Monoid

import System.IO
import System.Process
import XMonad
import XMonad.Core
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Layout
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.WorkspaceDir
import XMonad.Prompt
import XMonad.Util.CustomKeys
import XMonad.Util.EZConfig
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Timer
import qualified XMonad.Util.ExtensibleState as XS
import qualified XMonad.StackSet as W

-- some info about the screen
-- could probably be gotten from xrandr if i wasnt a lazy piece of shit
screenCharWidth = 205
screenPixelHeight = 900

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
myTerminal = "urxvt"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
myBorderWidth = 2

-- modMask lets you specify which modkey you want to use. The default is
-- mod1Mask ("left alt").  You may also consider using mod3Mask ("right alt"),
-- which does not conflict with emacs keybindings. The "windows key" is usually
-- mod4Mask.
myModMask = mod1Mask

-- The default number of workspaces (virtual screens) and their names.  By
-- default we use numeric strings, but any string may be used as a workspace
-- name. The number of workspaces is determined by the length of this list.
myWorkspaces = map show [1..10]

-- Color scheme
myBackgroundColor = "#151515"
myNormalColor     = "#d0d0d0"
myFocusedColor    = "#6fc2ef"
myUrgentColor     = "#ff0000"

---- KEY BINDINGS --------------------------------------------------------------

myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

  -- launch a terminal
  [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

  -- launch dmenu
  , ((modm,               xK_p     ), spawn "dmenu_run")

  -- launch gmrun
  , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

  -- close focused window
  , ((modm .|. shiftMask, xK_c     ), kill)

   -- Rotate through the available layout algorithms
  , ((modm,               xK_space ), sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default
  , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size
  , ((modm,               xK_n     ), refresh)

  -- Move focus to the next window
  , ((modm,               xK_Tab   ), windows W.focusDown)

  -- Move focus to the next window
  , ((modm,               xK_j     ), windows W.focusDown)

  -- Move focus to the previous window
  , ((modm,               xK_k     ), windows W.focusUp  )

  -- Move focus to the master window
  , ((modm,               xK_m     ), windows W.focusMaster  )

  -- Swap the focused window and the master window
  , ((modm,               xK_Return), windows W.swapMaster)

  -- Swap the focused window with the next window
  , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

  -- Swap the focused window with the previous window
  , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

  -- Shrink the master area
  , ((modm,               xK_h     ), sendMessage Shrink)

  -- Expand the master area
  , ((modm,               xK_l     ), sendMessage Expand)

  -- Push window back into tiling
  , ((modm,               xK_t     ), withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area
  , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

  -- Deincrement the number of windows in the master area
  , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap
  -- Use this binding with avoidStruts from Hooks.ManageDocks.
  -- See also the statusBar function from Hooks.DynamicLog.
  , ((modm              , xK_b     ), sendMessage ToggleStruts)

  -- Quit xmonad
  , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

  -- Restart xmonad
  , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

  -- Change the working directory in the workspace (from XMonad.Layout.WorkspaceDir)
  , ((modm .|. shiftMask, xK_x     ), changeDir defaultXPConfig)
  ]
  ++

  --
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  --
  [((m .|. modm, k), windows $ f i)
    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  --
  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  --
  [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
    | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


---- MOUSE BINDINGS ------------------------------------------------------------

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

  -- mod-button1, Set the window to floating mode and move by dragging
  [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

  -- mod-button2, Raise the window to the top of the stack
  , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

  -- mod-button3, Set the window to floating mode and resize by dragging
  , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

  -- you may also bind events to the mouse scroll wheel (button4 and button5)
  ]

---- LAYOUTS -------------------------------------------------------------------
-- You can specify and transform your layouts by modifying these values.  If you
-- change layout bindings be sure to use 'mod-shift-space' after restarting
-- (with 'mod-q') to reset your layout state to the new defaults, as xmonad
-- preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||, which
-- denotes layout choice.

myLayout = workspaceDir "~" $ avoidStruts $ smartBorders $ Full ||| tiled ||| Mirror tiled
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

---- WINDOW RULES --------------------------------------------------------------
-- Execute arbitrary actions and WindowSet manipulations when managing a new
-- window. You can use this to, for example, always float a particular program,
-- or have a client always appear on a particular workspace.
--
-- To find the property name associated with a program, use > xprop | grep
-- WM_CLASS and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that 'className'
-- and 'resource' are used below.

myManageHook = insertPosition Below Newer <+> manageDocks <+> composeAll
  [ className =? "MPlayer"        --> doFloat
  , className =? "Gimp"           --> doFloat
  , resource  =? "desktop_window" --> doIgnore
  , resource  =? "kdesktop"       --> doIgnore ]

---- EVENT HANDLING ------------------------------------------------------------
-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should return
-- (All True) if the default handler is to be run afterwards. To combine event
-- hooks use mappend or mconcat from Data.Monoid.

-- copied from http://stackoverflow.com/questions/11045239/can-xmonads-loghook-be-run-at-set-intervals-rather-than-in-merely-response-to
-- to force the clock to update at least once a second

-- wrapper for the Timer id, so it can be stored as custom mutable state
data TidState = TID TimerId deriving Typeable

instance ExtensionClass TidState where
  initialValue = TID 0

-- put this in your startupHook
-- start the initial timer, store its id
clockStartupHook = startTimer 1 >>= XS.put . TID

-- put this in your handleEventHook
clockEventHook e = do               -- e is the event we've hooked
  (TID t) <- XS.get                 -- get the recent Timer id
  handleTimer t e $ do              -- run the following if e matches the id
    startTimer 1 >>= XS.put . TID   -- restart the timer, store the new id
    ask >>= logHook.config          -- get the loghook and run it
    return Nothing                  -- return required type
  return (All True)                 -- return required type

-- /copied

myEventHook = clockEventHook

---- STATUS BARS AND LOGGING ---------------------------------------------------
-- Perform an arbitrary action on each internal state change or X event. See the
-- 'XMonad.Hooks.DynamicLog' extension for examples.

-- using the state of the X monad, prints basic data like workspaces
statusLog :: X String
statusLog = do
  current <- gets (W.workspace . W.current . windowset)
  hidden  <- gets (W.hidden . windowset)

  let hiddenTags = map W.tag $ filter (\x -> isJust $ W.stack x) hidden
  let currentTag = W.tag current

  let workspaces = concat $ map (' ':) $ (map (dzenColor myFocusedColor myBackgroundColor) $ sort $ filter (<currentTag) hiddenTags)
                                      ++ [wrap "[" "]" $ currentTag]
                                      ++ (map (dzenColor myFocusedColor myBackgroundColor) $ sort $ filter (>currentTag) hiddenTags)

  currentTime <- liftIO $ liftM (pad . formatTime defaultTimeLocale "%A %F %X %Z") getZonedTime

  currentTitle <- case W.stack current of
    Just st -> liftM (" : " ++) $ runQuery title $ W.focus st
    Nothing -> return ""

  let padding = take (screenCharWidth
              - (length $ dzenStrip workspaces)
              - (length $ dzenStrip currentTitle)
              - (length $ dzenStrip currentTime)) $ repeat ' '

  return (workspaces ++ currentTitle ++ padding ++ currentTime)

-- using X, prints all windows in the current workspace
windowLog :: X String
windowLog = do
  windows <- gets (W.stack . W.workspace . W.current . windowset)

  case windows of
    Just stack -> let windowCount = (length $ W.up stack) + 1 + (length $ W.down stack)
                      getTitle w = (runQuery title w)
                  in do
                    upTitles   <- liftM reverse $ mapM getTitle (W.up stack)
                    downTitles <- mapM getTitle (W.down stack)
                    focusTitle <- getTitle (W.focus stack)

                    let titles  = upTitles ++ [focusTitle] ++ downTitles

                    let longest = maximumBy (comparing length) :: [[a]] -> [a]

                    let shorten titles
                          | totalLength <= screenCharWidth = titles
                          | otherwise =
                              let maxIndex = snd . last . sort $ zip (map length titles) [0..]
                                  shortenTitle t = (take (length t - 4) t) ++ "..."
                              in  shorten $ Fold.toList $ Seq.adjust shortenTitle maxIndex (Seq.fromList titles)
                          where totalLength = (sum $ map length titles) + (4 * (length titles))

                    let shortened = shorten titles

                    let colored = map (dzenColor myNormalColor myBackgroundColor) (take (length upTitles) shortened)
                               ++ [(dzenColor myFocusedColor myBackgroundColor) (shortened !! (length upTitles))]
                               ++ map (dzenColor myNormalColor myBackgroundColor) (drop (length upTitles + 1) shortened)

                    let padded = map (\x -> wrap " [" "] " x) colored

                    return $ concat padded
    Nothing -> return ""

-- takes an X string and handle, writes the string to the handle
hookFrom :: X String -> Handle -> X ()
hookFrom x h = x >>= \s -> liftIO (hPutStrLn h s)

---- STARTUP HOOK --------------------------------------------------------------
-- Perform an arbitrary action each time xmonad starts or is restarted with
-- mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize per-workspace
-- layout choices.

myStartupHook = clockStartupHook

---- MAIN ----------------------------------------------------------------------
-- Now run xmonad with all the options we set up.

main = do
  let dzenProc = "dzen2 -ta l -fn 'inconsolata:size=10:antialias=true'" ++ " -fg '" ++ myNormalColor ++ "' -bg '" ++ myBackgroundColor ++ "'"
  dzenTop    <- spawnPipe (dzenProc ++ " -y 0")
  dzenBottom <- spawnPipe (dzenProc ++ " -y " ++ (show screenPixelHeight))
  xmonad $ defaultConfig {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    clickJustFocuses   = myClickJustFocuses,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalColor,
    focusedBorderColor = myFocusedColor,

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = myLayout,
    manageHook         = myManageHook,
    handleEventHook    = myEventHook,
    logHook            = hookFrom statusLog dzenBottom <+> hookFrom windowLog dzenTop,
    startupHook        = myStartupHook
  }
