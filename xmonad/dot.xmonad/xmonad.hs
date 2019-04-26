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
import Data.Monoid
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Layouts
-- import XMonad.Layout.StackTile
import XMonad.Layout.Accordion

-- Utilities for keybindings
import XMonad.Util.NamedActions
import XMonad.Util.EZConfig

-- for status bar
import XMonad.Hooks.ManageDocks
-- for xmobar
import XMonad.Util.Run(spawnPipe)
import XMonad.Hooks.DynamicLog

-- for taffybar
import           XMonad.Hooks.EwmhDesktops        (ewmh)
-- import           System.Taffybar.Support.PagerHints (pagerHints)

import System.Directory (getHomeDirectory)
import Text.Printf -- string formatting
-- for using xmonad with a destop environment
import XMonad.Config.Desktop

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--

main = do
  fixKbdSetup
  spawn myStatusbar
  xmonad
    $ ewmh
    $ docks 
    $ addDescrKeys' ((myModMask, xK_F1), showKeybindings) myKeys
    $ myConf


myConf = desktopConfig {
  layoutHook         = myLayout ||| layoutHook desktopConfig
  , terminal           = myTerminal
  , modMask            = myModMask
  }

-- manage xmobar as a bar (don't overrun it etc...)
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "gnome-terminal"

xkbcmd localXkbConfDir = printf "xkbcomp -I%s %s/keymap/custom :0 " localXkbConfDir localXkbConfDir -- $DISPLAY""
fixKbdSetup = do
  hd <- getHomeDirectory
  spawn $ xkbcmd $ hd ++ "/dotfiles/i3/xkbconf"
------------------------------------------------------------------------
myModMask       = mod3Mask
myWorkspaces    = map show [1..9]
------------------------------------------------------------------------
showKeybindings :: [((KeyMask, KeySym), NamedAction)] -> NamedAction
showKeybindings x = addName "Show Keybindings" $ io $ do
    h <- spawnPipe "zenity --text-info --font=terminus"
    hPutStr h (unlines $ showKm x)
    hClose h
    return ()

myKeys conf = mkNamedKeymap conf
  [ ("M-RET", spawn' $ myTerminal)
  , ("M-p"  , spawn' "dmenu_run")
  , ("M-<Backspace>", addName "Close Window" $ kill)
  , ("M-<Space>", sendMessage' NextLayout)
  , ("M-S-<Space>", addName "Default layout" $ setLayout $ XMonad.layoutHook conf)
  , ("M-n", addName "Refresh" $ refresh)
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
  , ("C-S-k", spawn' $ xkbcmd "/home/nivpgir/.config/i3/xkbconf")
  ]
  -- ++ [
  --
  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  --
  -- ("M-" ++ secondMod ++ k, windows $ f i) | (i, k) <- zip (XMonad.workspaces conf) (map show [1..9])
  --                                     , (f, secondMod) <- [(W.greedyView, ""), (W.shift, "S-")]]
  -- -- ("M-F1", spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
  --
  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  --
  -- [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
  -- | (key, sc) <- zip [xK_z, xK_x, xK_c] [0..]
  -- , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


-- myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

--     -- launch a terminal
--     [ ((modm, xK_Return), spawn $ XMonad.terminal conf)

--     -- launch dmenu
--     , ((modm,               xK_p     ), spawn "dmenu_run")

--     -- close focused window
--     , ((modm, xK_BackSpace     ), kill)

--      -- Rotate through the available layout algorithms
--     , ((modm,               xK_space ), sendMessage NextLayout)

--     --  Reset the layouts on the current workspace to default
--     , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

--     -- Resize viewed windows to the correct size
--     , ((modm,               xK_n     ), refresh)

--     -- Move focus to the next window
--     , ((modm,               xK_d     ), windows W.focusDown)

--     -- Move focus to the previous window
--     , ((modm,               xK_s     ), windows W.focusUp  )

--     -- Move focus to the master window
--     , ((modm,               xK_a     ), windows W.focusMaster  )

--     -- Swap the focused window with the next window
--     , ((modm .|. shiftMask, xK_d     ), windows W.swapDown  )

--     -- Swap the focused window with the previous window
--     , ((modm .|. shiftMask, xK_s     ), windows W.swapUp    )

--     -- Swap the focused window and the master window
--     , ((modm .|. shiftMask, xK_a), windows W.swapMaster)

--     -- Shrink the master area
--     , ((modm,               xK_minus     ), sendMessage Shrink)

--     -- Expand the master area
--     , ((modm,               xK_equal     ), sendMessage Expand)

--     -- Push window back into tiling
--     , ((modm,               xK_t     ), withFocused $ windows . W.sink)

--     -- Increment the number of windows in the master area
--     , ((modm              , xK_m ), sendMessage (IncMasterN 1))

--     -- Deincrement the number of windows in the master area
--     , ((modm              , xK_l), sendMessage (IncMasterN (-1)))

--     -- Toggle the status bar gap
--     -- Use this binding with avoidStruts from Hooks.ManageDocks.
--     -- See also the statusBar function from Hooks.DynamicLog.
--     --
--     -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

--     -- Quit xmonad
--     , ((modm .|. shiftMask, xK_Escape     ), io (exitWith ExitSuccess))

--     -- Restart xmonad
--     , ((modm .|. controlMask, xK_c     ), spawn "xmonad --recompile && xmonad --restart")
--     , ((controlMask .|. shiftMask, xK_k), spawn $ xkbcmd "/home/nivpgir/.config/i3/xkbconf")
--     -- Run xmessage with a summary of the default keybindings (useful for beginners)
--     , ((modm, xK_F1 ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
--     , ((mod1Mask, xK_F2 ), spawn ("echo \"" ++ "mod1" ++ "\" | xmessage -file -"))
--     , ((mod3Mask, xK_F2 ), spawn ("echo \"" ++ "mod3" ++ "\" | xmessage -file -"))
--     , ((mod4Mask, xK_F2 ), spawn ("echo \"" ++ "mod4" ++ "\" | xmessage -file -"))    
--     ]
--     ++

--     --
--     -- mod-[1..9], Switch to workspace N
--     -- mod-shift-[1..9], Move client to workspace N
--     --
--     [((m .|. modm, k), windows $ f i)
--         | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
--         , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
--     ++

--     --
--     -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
--     -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
--     --
--     [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
--         | (key, sc) <- zip [xK_z, xK_x, xK_c] [0..]
--         , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

myLayout = avoidStruts $ Accordion ||| tiled ||| Mirror tiled ||| Full
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall nmaster delta ratio
    nmaster = 1
    ratio   = 4/7 -- Default proportion of screen occupied by master pane
    delta   = 3/100 -- Percent of screen to increment by when resizing panes

myStatusbar = "my-taffybar"
