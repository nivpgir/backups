--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

-------------
-- Imports --
-------------

import System.IO
import XMonad
import System.Exit
import qualified XMonad.StackSet as W
import XMonad.Layout.Spacing    -- Window gaps
import XMonad.Layout.Accordion  -- Layouts
import XMonad.Util.NamedActions -- Utilities for keybindings;
import XMonad.Util.EZConfig
import XMonad.Hooks.ManageDocks -- for status bar
import XMonad.Util.Run(spawnPipe) -- for xmobar
import XMonad.Hooks.EwmhDesktops        (ewmh) -- for taffybar
import System.Directory (getHomeDirectory)
import Text.Printf -- string formatting
import XMonad.Config.Desktop -- for using xmonad with a destop environment
import XMonad.Hooks.DynamicLog  -- dzen
import XMonad.Util.Loggers  -- dzen
import XMonad.Actions.Navigation2D -- up down left right
-- import XMonad.Action.GridSelect
main = do
  fixKbdSetup
  -- with this the laptop screen closes when lid is closed
  spawn "xrandr --output eDP-1 --auto"
  spawn "stalonetray"
  d <- spawnPipe myStatusbar
  xmonad
    $ withNavigation2DConfig nav2DConf $ additionalNav2DKeysP ("w", "a", "s", "d") [("M-", windowGo), ("M-S-", windowSwap)] False 
    $ ewmh
    $ docks 
    $ addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys
    $ desktopConfig {
    layoutHook = avoidStruts $ myLayout
    , logHook = myLogHook d
    , manageHook = manageDocks
    , terminal = myTerminal
    , modMask = myModMask
    , focusFollowsMouse = False
    }

-- myLogHook h = dynamicLogWithPP $ byorgeyPP
myLogHook h = dynamicLogWithPP $ defaultPP
    -- display current workspace as darkgrey on light grey (opposite of default colors)
    { ppCurrent         = dzenColor "#303030" "#909090" . pad 
    -- display other workspaces which contain windows as a brighter grey
    , ppHidden          = dzenColor "#909090" "" . pad 
    -- display other workspaces with no windows as a normal grey
    , ppHiddenNoWindows = dzenColor "#606060" "" . pad 
    -- display the current layout as a brighter grey
    , ppLayout          = dzenColor "#909090" "" . pad 
    -- if a window on a hidden workspace needs my attention, color it so
    , ppUrgent          = dzenColor "#ff0000" "" . pad . dzenStrip
    -- shorten if it goes over 100 characters
    , ppTitle           = shorten 100
    -- no separator between workspaces
    , ppWsSep           = ""
    -- put a few spaces between each object
    , ppSep             = "  "
    -- output to the handle we were given as an argument
    , ppOutput          = hPutStrLn h
    , ppExtras           = [ date "%D %T"
                           , loadAvg]
    }
    -- dzenColor "green"
    
-- myConf logHookPipe = desktopConfig {
--   layoutHook = avoidStruts $ myLayout
--   -- , logHook = myLogHook logHookPipe
--   , manageHook = manageDocks
--   , terminal = myTerminal
--   , modMask = myModMask
--   }
------------------------------------------------------------------------
myTerminal      = "gnome-terminal"
------------------------------------------------------------------------
xkbcmd localXkbConfDir = printf "xkbcomp -I%s %s/keymap/custom :0 " localXkbConfDir localXkbConfDir -- $DISPLAY""
fixKbdSetup = do
  hd <- getHomeDirectory
  spawn $ xkbcmd $ hd ++ "/dotfiles/i3/xkbconf"
------------------------------------------------------------------------
myModMask       = mod3Mask
------------------------------------------------------------------------
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe "zenity --text-info --font=terminus"
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()



nav2DConf = def { defaultTiledNavigation = centerNavigation
                , layoutNavigation   = [("Full", centerNavigation)]
                , unmappedWindowRect = [("Full", singleWindowRect)]
                }
myKeys conf = mkNamedKeymap conf $
  [ ("M-<Return>", spawn' $ myTerminal)
  , ("M-p"  , spawn' "dmenu_run")
  , ("M-<Backspace>", addName "Close Window" $ kill)
  , ("M-<Space>", sendMessage' NextLayout)
  , ("M-S-<Space>", addName "Default layout" $ setLayout $ XMonad.layoutHook conf)
  , ("M-n", addName "Refresh" $ refresh)
    -- Switch between layers
  , ("M-f", addName "Switch Layer" $ switchLayer)
  -- Directional navigation of windows
  -- , ("M-d", addName "windowGo R" $ windowGo R False)
  -- , ("M-a", addName "windowGo L" $ windowGo L False)
  -- , ("M-w", addName "windowGo U" $ windowGo U False)
  -- , ("M-s", addName "windowGo D" $ windowGo D False)
  
  -- , ("M-S-d", addName "windowSwap R" $ windowSwap R False)
  -- , ("M-S-a", addName "windowSwap L" $ windowSwap L False)
  -- , ("M-S-w", addName "windowSwap U" $ windowSwap U False)
  -- , ("M-S-s", addName "windowSwap D" $ windowSwap D False)
  
  -- , ("M-C-d", addName "windowToScreen R" $ windowToScreen R False)
  -- , ("M-C-a", addName "windowToScreen L" $ windowToScreen L False)
  -- , ("M-C-w", addName "windowToScreen U" $ windowToScreen U False)
  -- , ("M-C-s", addName "windowToScreen D" $ windowToScreen D False)
  
    -- Send window to adjacent screen
  -- , ((modm .|. mod1Mask,    xK_r    ), windowToScreen R False)
  -- , ((modm .|. mod1Mask,    xK_l    ), windowToScreen L False)
  -- , ((modm .|. mod1Mask,    xK_u    ), windowToScreen U False)
  -- , ((modm .|. mod1Mask,    xK_d    ), windowToScreen D False)
  , ("M-d", addName "Focus next window" $ windows W.focusDown)
  , ("M-s", addName "Focus previos window" $ windows W.focusUp)
  , ("M-a", addName "Focus Master" $ windows W.focusMaster)
  , ("M-S-d", addName "Swap window with next" $ windows W.swapDown)
  , ("M-S-s", addName "Swap window with previous" $ windows W.swapUp)
  , ("M-S-a", addName "Swap with master" $ windows W.swapMaster)
  , ("M--", sendMessage' Shrink)
  , ("M-<Equal>", sendMessage' Expand)
  , ("M-t", addName "Push window back to tiling" $ withFocused $ windows . W.sink)
  , ("M-m", sendMessage' (IncMasterN 1))
  , ("M-l", sendMessage' (IncMasterN (-1)))
  , ("M-S-<Escape>", addName "Exit" $ io (exitWith ExitSuccess))
  , ("M-C-c", spawn' "xmonad --recompile && xmonad --restart")
  , ("C-S-k", spawn' $ xkbcmd "/home/nivpgir/dotfiles/i3/xkbconf")
  ]
  ++
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [("M-" ++ secondMod ++ k, addName ("Move to / Move window to workspace " ++ k) $ windows $ f i)
  | (i, k) <- zip (XMonad.workspaces conf) (map show [1..9])
  , (f, secondMod) <- [(W.greedyView, ""), (W.shift, "S-")]]
  ++
  -- mod-{z,x,c}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{z,x,c}, Move client to screen 1, 2, or 3
  [ ("M-"++secondMod++key , addName ("change/move screen to" ++ key) (screenWorkspace sc >>= flip whenJust (windows . f)))
  | (key, sc) <- zip ["z", "x", "c"] [0..]
  , (f, secondMod) <- [(W.view, ""), (W.shift, "S-")]]
------------------------------------------------------------------------

------------------------------------------------------------------------
myLayout = spacingRaw True (Border 0 screenGap screenGap screenGap) True (Border 0 windowsGap windowsGap windowsGap) True $
   Accordion ||| tiled ||| Mirror tiled ||| Full
  where
    windowsGap = 5
    screenGap = 1
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 4/7 -- Default proportion of screen occupied by master pane
    delta   = 3/100 -- Percent of screen to increment by when resizing panes

myStatusbar =  "dzen2 -dock -p -xs 1 -ta l -e 'onstart=lower'"
