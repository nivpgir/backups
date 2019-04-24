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
    $ desktopConfig {
    manageHook           = myManageHook <+> manageHook desktopConfig
    , layoutHook         = myLayout ||| layoutHook desktopConfig
    , keys               = myKeys
    , mouseBindings      = myMouseBindings
    , terminal           = myTerminal
    , borderWidth        = myBorderWidth
    , modMask            = myModMask
    , startupHook        = myStartupHook
    -- , logHook            = myLogHook xmproc
    }

-- manage xmobar as a bar (don't overrun it etc...)
-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "termite"

xkbcmd locXkbConfDir = printf "xkbcomp -I%s %s/keymap/custom :0 " locXkbConfDir locXkbConfDir -- $DISPLAY""
fixKbdSetup = do
  hd <- getHomeDirectory
  spawn $ xkbcmd $ hd ++ "/.config/i3/xkbconf"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 2

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--

myModMask       = mod3Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]
-- myWorkspaces    = map show [1..]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"


------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
-- 
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

    -- close focused window
    , ((modm, xK_BackSpace     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_d     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_s     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_a     ), windows W.focusMaster  )

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_d     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_s     ), windows W.swapUp    )

    -- Swap the focused window and the master window
    , ((modm .|. shiftMask, xK_a), windows W.swapMaster)

    -- Shrink the master area
    , ((modm,               xK_minus     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_equal     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_m ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_l), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_Escape     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm .|. controlMask, xK_c     ), spawn "xmonad --recompile && xmonad --restart")
    , ((controlMask .|. shiftMask, xK_k), spawn $ xkbcmd "/home/nivpgir/.config/i3/xkbconf")
    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm, xK_F1 ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))
    , ((mod1Mask, xK_F2 ), spawn ("echo \"" ++ "mod1" ++ "\" | xmessage -file -"))
    , ((mod3Mask, xK_F2 ), spawn ("echo \"" ++ "mod3" ++ "\" | xmessage -file -"))
    , ((mod4Mask, xK_F2 ), spawn ("echo \"" ++ "mod4" ++ "\" | xmessage -file -"))    
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
        | (key, sc) <- zip [xK_z, xK_x, xK_c] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
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

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts $ Mirror Accordion ||| tiled ||| Mirror tiled ||| Full
-- ||| StackTile 1 (3/100) (1/2)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled = Tall nmaster delta ratio

      -- The default number of windows in the master pane
    nmaster = 1

     -- Default proportion of screen occupied by master pane
    ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
    delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = manageDocks <+> composeAll
               [ className =? "MPlayer"        --> doFloat
               , className =? "Gimp"           --> doFloat
               , resource  =? "desktop_window" --> doIgnore
               , resource  =? "kdesktop"       --> doIgnore ]

-- myManageHook = manageDocks <+> manageHook defaultConfig
------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--

-- ewmhDesktopsEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook p = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn p
            , ppTitle = xmobarColor "green" "" . shorten 50
            }


------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = do
  spawn "xkbcomp -synch -w3 -I$HOME/xkbconf/xkb/ $HOME/xkbconf/xkb/keymap/custom.xkb $DISPLAY"
  spawn "systemctl --user restart xcape.service"
--  return ()

myStatusbar = "my-taffybar"


-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = unlines ["The default modifier key is 'alt'. Default keybindings:",
    "",
    "-- launching and killing programs",
    "mod-Shift-Enter  Launch xterminal",
    "mod-p            Launch dmenu",
    "mod-backspace      Close/kill the focused window",
    "mod-Space        Rotate through the available layout algorithms",
    "mod-Shift-Space  Reset the layouts on the current workSpace to default",
    "mod-n            Resize/refresh viewed windows to the correct size",
    "",
    "-- move focus up or down the window stack",
    "mod-Tab        Move focus to the next window",
    "mod-Shift-Tab  Move focus to the previous window",
    "mod-e          Move focus to the next window",
    "mod-q          Move focus to the previous window",
    "mod-r          Move focus to the master window",
    "",
    "-- modifying the window order",
    "mod-Tab   Swap the focused window and the master window",
    "mod-Shift-e  Swap the focused window with the next window",
    "mod-Shift-q  Swap the focused window with the previous window",
    "",
    "-- resizing the master/slave ratio",
    "mod-minus  Shrink the master area",
    "mod-plus  Expand the master area",
    "",
    "-- floating layer support",
    "mod-t  Push window back into tiling; unfloat and re-tile it",
    "",
    "-- increase or decrease number of windows in the master area",
    "mod-m    Increment the number of windows in the master area",
    "mod-l    Deincrement the number of windows in the master area",
    "",
    "-- quit, or restart",
    "mod-Shift-q  Quit xmonad",
    "mod-c        Restart xmonad",
    "mod-[1..9]   Switch to workSpace N",
    "",
    "-- Workspaces & screens",
    "mod-Shift-[1..9]   Move client to workspace N",
    "mod-{z,x,c}        Switch to physical/Xinerama screens 1, 2, or 3",
    "mod-Shift-{z,x,c}  Move client to screen 1, 2, or 3",
    "",
    "-- Mouse bindings: default actions bound to mouse events",
    "mod-button1  Set the window to floating mode and move by dragging",
    "mod-button2  Raise the window to the top of the stack",
    "mod-button3  Set the window to floating mode and resize by dragging"]
