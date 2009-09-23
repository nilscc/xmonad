----------------------------------------------------------------------
--                                                                  --
-- XMonad Settings                                                  --
--                                                                  --
--                                                                  --
-- XMonad version 0.8.1-darcs                                       --
-- XMonad config: 2009-09-06                                        --
--                                                                  --
-- by Nils                                                          --
--                                                                  --
-- For questions, write an email to:                                --
-- mail (at) n-sch . de                                             --
-- Or visit the #xmonad channel at irc.freenode.net                 --
--                                                                  --
--                                                                  --
-- Additional files:                                                --
-- http://www.n-sch.de/xmonad/icons.zip                             --
-- http://www.n-sch.de/xmonad/conkytoprc                            --
--                                                                  --
--                                                                  --
-- For the conkybar you need conky-cli or compile it with the       --
--     --disable-x11 option.                                        --
-- Notice: M-Shift-R will killall conky before restarting XMonad!   --
--                                                                  --
-- To make "clickable" work with dzen2, you need the                --
-- latest svn version.                                              --
--                                                                  --
--                                                                  --
-- Edit with VIM ":set foldmethod=marker" to use nice folding :)    --
--                                                                  --
--                                                                  --
-- {{{ Basic usage:                                                 --
--                                                                  --
-- M  = Mod4Mask                                                    --
-- M1 = Mod1Mask "Alt"                                              --
-- S  = ShiftMask                                                   --
-- C  = ControlMask                                                 --
--                                                                  --
-- Applications:                                                    --
--                                                                  --
-- M + F1               - Start urxvtc                              --
-- M + F2               - Start dmenu                               --
-- M + F3               - Start thunar                              --
-- M + F4               - Start opera                               --
-- M + F5               - Start irssi in urxvtc                     --
-- M + F6               - Start pidgin on last workspace            --
-- M + F7               - Start mutt in urxvtc                      --
-- M + F8               - Start ncmpcpp in urxvtc                   --
-- M + S + F4           - Start liferea                             --
-- M + S + F7           - Start abook in urxvtc                     --
-- M + S + F8           - Start gmpc                                --
-- Mediakeys            - Control MPD & OSS                         --
--                                                                  --
-- M + Escape           - Kill current window                       --
--                                                                  --
-- WM control:                                                      --
--                                                                  --
-- M + j/k              - Go up/down in window stack                --
-- M + h/l              - Adjust width/height of master             --
-- M + S + h/l          - Adjust width/height of slaves             --
-- M + ,/.              - Adjust numbers of master windows          --
-- M + S + n            - Refresh current window                    --
--                                                                  --
-- M + q                - View urgent workspace                     --
-- M + w                - Focus master window                       --
-- M + S + w            - Swap current window with master           --
-- M + e                - Move to next empty workspace              --
-- M + S + e            - Shift current window to next empty ws     --
--                                                                  --
-- M + Space            - Next layout                               --
-- M + S + Space        - Reset layout                              --
--                                                                  --
-- M + 1-0              - View workspace x or toggle to previous ws --
-- M + M1 + 1-0         - View workspace x on screen 0 *            --
-- M + C + 1-0          - View workspace x on screen 1 *            --
-- M + S + 1-0          - Move current window to workspace x        --
-- M + S + C + 1-0      - Default greedyView behaviour              --
--                                                                  --
-- M + x / <R>          - Next workspace                            --
-- M + y / <L>          - Previous workspace                        --
-- M + MousewheelDOWN   - Next workspace                            --
-- M + MousewheelUP     - Previous workspace                        --
--                                                                  --
-- M + c                - Copy current window to all workspaces     --
-- M + S + c            - Kill all other copies                     --
--                                                                  --
-- M + f                - Toggle fullscreen                         --
-- M + Leftclick        - Float window                              --
-- M + Rightclick       - Adjust size of floated window             --
-- M + t                - Push window back into tiling              --
-- M + S + t            - Push all windows back into tiling         --
-- M + tab              - Cycle through recent (hidden) workspaces  --
--                        tab - next workspace                      --
--                        esc - previous workspace                  --
--                        release mod to exit cycle mode            --
--                                                                  --
-- Xinerama support:                                                --
--                                                                  --
-- M + a                - Switch focus to next screen               --
-- M + S + a            - Switch focus to previous screen           --
-- M + s                - Swap current screen with next screen      --
-- M + S + s            - Swap current screen with previous screen  --
--                                                                  --
-- XMonad stuff:                                                    --
--                                                                  --
-- M + S + r            - Reload XMonad                             --
-- M + C + Backspace    - Exit XMonad                               --
--                                                                  --
--                                                                  --
--                                                                  --
-- * Comment:                                                       --
-- M + ^ is basicly an alias for M + 0 (the last workspace)         --
--                                                                  --
-- }}}                                                              --
----------------------------------------------------------------------


-- {{{ imports

-- Core
import XMonad

import Data.Monoid
import Data.Maybe (fromMaybe)
import Data.Ratio ((%))
import System.IO
import System.Exit

import Control.Monad (liftM2, ap)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified Data.List       as L

-- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.OnScreen
import XMonad.Actions.SinkAll
import XMonad.Actions.WindowGo

-- Utils
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.EZConfig

-- Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetCursor
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive

-- Layouts
import XMonad.Layout.Reflect
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LayoutHints
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.SimplestFloat

import XMonad.Layout.ResizableTile
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.IM



import XMonad.Layout.Tabbed

-- }}}

-- {{{ Run XMonad

main = do
    dzen    <- spawnPipe myStatusBar
    conky   <- spawnPipe myConkyBar
    xmonad $ withUrgencyHookC NoUrgencyHook (urgencyConfig { suppressWhen = Focused }) defaultConfig {

        -- simple stuff
          terminal           = myTerminal
        , focusFollowsMouse  = myFocusFollowsMouse
        , borderWidth        = myBorderWidth
        , modMask            = myModMask
        , workspaces         = myWorkspaces
        , normalBorderColor  = myNormalBorderColor
        , focusedBorderColor = myFocusedBorderColor

        -- key bindings
        , keys               = myKeys
        , mouseBindings      = myMouseBindings

        -- hooks, layouts
        , layoutHook         = myLayout
        , manageHook         = myManageHook
        , handleEventHook    = myEventHook
        , logHook            = myLogHook dzen
        , startupHook        = myStartupHook

    }

-- }}}

-- {{{ Settings

myTerminal              = "urxvtc"

myModMask               = mod4Mask

myFocusFollowsMouse     = True

myWorkspaces            = clickable . (map dzenEscape) $ nWorkspaces 9 ["web", "irc", "com"]

  where nWorkspaces n []= map show [1 .. n]
        nWorkspaces n l = init l ++ map show [length l .. n] ++ [last l]
        clickable l     = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()"
                          | (i,ws) <- zip [1..] l
                          , let n = if i == 10 then 0 else i
                          ]

-- }}}

-- {{{ Appearance

-- Borders & Separators
myBorderWidth           = 2
myNormalBorderColor     = "#000000"
myFocusedBorderColor    = "#406020"

mySep                   = "|"
myWorkspaceSep          = ""

-- Font - change, if you dont have artwiz fonts
myFont                  = "-*-snap-*-*-*-*-*-*-*-*-*-*-*-*"

-- Icons
-- get icons at http://www.n-sch.de/xmonad/icons.zip
myIconDir                           = "/home/nils/.xmonad/icons"
myWsIcon                            = "corner.xbm"

myIcons "ResizableTall"             = Just "layout-tall-right.xbm"
myIcons "Mirror ResizableTall"      = Just "layout-mirror-bottom.xbm"
myIcons "Full"                      = Just "layout-full.xbm"
myIcons "IM Grid"                   = Just "layout-im.xbm"
myIcons "IM ResizableTall"          = Just "layout-im-tall.xbm"
myIcons "IM Mirror ResizableTall"   = Just "layout-im-mirror.xbm"
myIcons "IM Full"                   = Just "layout-im-full.xbm"
myIcons "IM ReflectX IM Full"       = Just "layout-gimp.xbm"

myIcons _                           = Nothing


-- Colors:

myDzenFGColor           = "#303030"
myDzenBGColor           = ""

myNormalFGColor         = "#77e000" -- "#324c80" -- "#e66900"
myNormalBGColor         = "black" -- "#000000"

myFocusedFGColor        = "#f0f0f0" -- "#b30a30" -- "#77f000"
myFocusedBGColor        = "#121212"

myUrgentFGColor         = "white"
myUrgentBGColor         = "#991133"

myVisibleFGColor        = "white"
myVisibleBGColor        = ""

myHiddenFGColor         = myNormalFGColor -- "#e66900" -- "white"
myHiddenBGColor         = ""

myEmptyFGColor          = "#444444"
myEmptyBGColor          = ""

-- Tabbed theme:

myTabbedTheme = Theme {

      activeTextColor       = myFocusedFGColor
    , activeColor           = myFocusedBGColor
    , activeBorderColor     = myNormalBorderColor
    , inactiveTextColor     = myNormalFGColor
    , inactiveColor         = myNormalBGColor
    , inactiveBorderColor   = myNormalBorderColor
    , urgentTextColor       = myUrgentFGColor
    , urgentColor           = myUrgentBGColor
    , urgentBorderColor     = myNormalBorderColor
    , fontName              = myFont
    , decoWidth             = 200
    , decoHeight            = 18

}

-- }}} Appearance

-- {{{ Key & Mouse

myKeys conf = mkKeymap conf $

    -- General moving & stuff
    [ ("M-<Escape>", kill)                           -- close focused window

    , ("M-<Space>", sendMessage NextLayout)     -- Rotate through the available layout algorithms
    , ("M-S-<Space>", setLayout $ XMonad.layoutHook conf) --  Reset the layouts on the current workspace to default

    , ("M-n", refresh)                          -- Resize viewed windows to the correct size

    , ("M-j", windows W.focusDown)              -- Move focus to the next window
    , ("M-<D>", windows W.focusDown)            -- Move focus to the next window
    , ("M-k", windows W.focusUp)                -- Move focus to the previous window
    , ("M-<U>", windows W.focusUp)              -- Move focus to the previous window

    , ("M-q", focusUrgent)                      -- Focus urgent windows
    , ("M-w"  , windows W.focusMaster)          -- Move focus to the master window
    , ("M-S-w", windows W.shiftMaster)          -- Swap the focused window and the master window
    , ("M-e"  , moveTo Next EmptyWS)            -- View next empty workspace
    , ("M-S-e", shiftTo Next EmptyWS)           -- View next empty workspace

    , ("M-S-j", windows W.swapDown)             -- Swap the focused window with the next window
    , ("M-S-k", windows W.swapUp)               -- Swap the focused window with the previous window

    , ("M-,", sendMessage $ IncMasterN 1)       -- Increment the number of windows in the master area
    , ("M-.", sendMessage $ IncMasterN (-1))    -- Deincrement the number of windows in the master area

    , ("M-h", sendMessage Shrink)               -- Shrink the master area
    , ("M-l", sendMessage Expand)               -- Expand the master area
    , ("M-S-l", sendMessage MirrorShrink)       -- Shrink slaves
    , ("M-S-h", sendMessage MirrorExpand)       -- Expand slaves

    , ("M-t", withFocused $ windows . W.sink)   -- Push window back into tiling
    , ("M-S-t", sinkAll)                        -- Push all floating windows back into tiling

    -- Toggle fullscreen
    , ("M-f", sendMessage ToggleStruts >> sendMessage ToggleLayout)

    -- Toggle previously displayed workspaces
    , ("M-<Tab>", myCycleRecentWS)

    -- Xinerama stuff:
    -- Switch focus on screens
    , ("M-a"    , nextScreen)
    , ("M-S-a"  , prevScreen)
    -- Swap screens
    , ("M-s"    , swapNextScreen)
    , ("M-S-s"  , swapPrevScreen)
    -- Switch to next/previous workspace
    , ("M-x"    , nextWS)
    , ("M-<R>"  , nextWS)
    , ("M-y"    , prevWS)
    , ("M-<L>"  , prevWS)

    -- Copy current window to all workspaces
    , ("M-c"    , windows copyToAll)
    , ("M-S-c"  , killAllOtherCopies)

    -- Restart XMonad, take care of conky
    , ("M-S-r", spawn "exec killall conky" >> restart "xmonad" True)
    -- Quit XMonad
    , ("M-C-<Backspace>", io $ exitWith ExitSuccess)
    ]
    ++

    -- Applications to run
    [ ("M-<F1>",    spawn myTerminal)
    , ("M-<F2>",    spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"") -- launch dmenu
    , ("M-<F3>",    spawn "exec thunar")
    -- , ("M-<F4>",    runOrRaise   "opera" (className =? "Opera"))
    , ("M-<F4>",    spawn "exec surf")
    , ("M-<F5>",    runOrRaise   "pidgin"  (className =? "Pidgin"))
    , ("M-<F6>",    myRunOrRaise (myTerminal ++ " -name irssi -e zsh -c \"screen -x || screen irssi\"") (resource =? "irssi"))
    , ("M-<F7>",    myRunOrRaise (myTerminal ++ " -name mutt -e mutt ") (resource =? "mutt"))
    , ("M-<F8>",    myRunOrRaise (myTerminal ++ " -name ncmpcpp -e ~/.scripts/ncmpcpp_with_host") (resource =? "ncmpcpp"))
    , ("M-S-<F4>",  runOrRaise   "liferea" (className =? "Liferea"))
    , ("M-S-<F7>",  myRunOrRaise (myTerminal ++ " -name abook -e abook ") (resource =? "abook"))
    , ("M-S-<F8>",  runOrRaise   "gmpc" (className =? "Gmpc"))
    ]
    ++

    -- Multimedia keys
    [ ("<XF86AudioMute>",        spawn "exec ossvol -t")
    , ("<XF86AudioRaiseVolume>", spawn "exec ossvol -i 1")
    , ("<XF86AudioLowerVolume>", spawn "exec ossvol -d 1")
    , ("<XF86AudioPlay>",        spawn "exec ~/.scripts/mpc_with_host toggle")
    , ("<XF86AudioStop>",        spawn "exec ~/.scripts/mpc_with_host stop")
    , ("<XF86AudioPrev>",        spawn "exec ~/.scripts/mpc_with_host prev")
    , ("<XF86AudioNext>",        spawn "exec ~/.scripts/mpc_with_host next")
    ]
    ++

    -- Control of workspaces
    -- Basicly M+1-0 and some modifiers. See the basic usage on top of this
    -- file for more information
    [ (m ++ k, f i)
         | (i, k) <- zip ((\ws -> last ws : ws) . workspaces $ conf)
                         ("^" : map show ([1..9] ++ [0]))
         , (m, f) <- [ ("M-"    , toggleOrView)
                     , ("M-M1-" , windows . viewOnScreen 0)
                     , ("M-C-"  , windows . viewOnScreen 1)
                     , ("M-S-"  , windows . liftM2 (.) W.view W.shift)
                     , ("M-C-S-", windows . W.greedyView)
                     ]
    ]

  where hiddenTags w    = map W.tag $ W.hidden w ++ [W.workspace . W.current $ w]
        visibleTags w   = map (W.tag . W.workspace) $ W.visible w ++ [W.current w]
        myRunOrRaise cmd qry = ifWindow qry raiseHook (spawn cmd) -- needed for terminal applications
        myCycleRecentWS = let options w = map (W.view `flip` w) (hiddenTags w)
                          in cycleWindowSets options [xK_Super_L] xK_Tab xK_q


myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)) -- Set the window to floating mode and move by dragging
    , ((modMask, button2), (\w -> focus w >> sendMessage ToggleStruts >> sendMessage ToggleLayout)) -- Toggle fullscreen
    --, ((modMask, button2), (\w -> focus w >> windows W.shiftMaster)) -- Raise the window to the top of the stack
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)) -- Set the window to floating mode and resize by dragging
    , ((modMask, button4), (\_ -> prevWS)) -- previous Workspace
    , ((modMask, button5), (\_ -> nextWS)) -- next Workspace

    ]

-- }}}

-- {{{ Statusbars and LogHook

-- Statusbar with workspaces, layout and title
myStatusBar = "dzen2 -x 0   -y 0 -h 18 -ta l -fg '" ++ myDzenFGColor ++
              "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "' -w 670"

-- Conky bar
myConkyBar  = "conky -c ~/.xmonad/conkytoprc | dzen2 -x 670 -y 0 -h 18 -ta r -fg '" ++ myDzenFGColor ++
              "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "' -w 770"

-- LogHook
myLogHook dzen = do
    fadeInactiveLogHook 0xdddddddd
    dynamicLogWithPP $ defaultPP {
          ppLayout          = dzenColor  myNormalFGColor  myNormalBGColor    . pad . loadIcons
        , ppTitle           = dzenColor  myNormalFGColor  myNormalBGColor    . pad . dzenEscape

        , ppCurrent	        = dzenColor  myFocusedFGColor myFocusedBGColor   . pad . cornerIcon
        , ppVisible	        = dzenColor  myVisibleFGColor myVisibleBGColor   . pad
        , ppHidden	        = dzenColor  myHiddenFGColor  myHiddenBGColor    . pad . cornerIcon
        , ppHiddenNoWindows = dzenColor  myEmptyFGColor   myEmptyBGColor     . pad
        , ppUrgent	        = dzenColor  myUrgentFGColor  myUrgentBGColor    . pad . cornerIcon -- . strip

        , ppWsSep           = dzenEscape myWorkspaceSep
        , ppSep             = dzenEscape mySep

	    , ppOutput          = hPutStrLn  dzen
        }

  where loadIcons s = fromMaybe s $ myIcons s >>= \icon -> return $ "^i(" ++ myIconDir ++ "/" ++ icon ++ ")"
        cornerIcon  = (++) $ "^i(" ++ myIconDir ++ "/" ++ myWsIcon ++ ")"
        strip       = tail . init . dzenStrip

-- }}}

-- {{{ Event, Startup and Manage hook

-- Event hook
myEventHook = const . return $ All True

-- Startup hook
myStartupHook = do
    setDefaultCursor 68
    setWMName "LG3D"

-- Manage hook
myManageHook = composeAll $

    -- Float apps
    [ className =? c <||> resource =? r <||> title =? t <||> isDialog --> doCenterFloat
    | c <- ["Wine", "Switch2"]
    , r <- ["Dialog", "Download"]
    , t <- ["Schriftart auswählen", "Choose a directory"]
    ] ++

    -- Workspaces
    [ className =? "Opera"      --> moveTo 0
    , className =? "surf"       --> moveTo 0
    , className =? "Xchat"      --> moveTo 1
    , resource  =? "irssi"      --> moveTo 1
    , className =? "Pidgin"     --> moveTo (-1)

    -- "Real" fullscreen
    , isFullscreen              --> doFullFloat 
    ]

  where moveTo i = doF . W.shift $ if i == -1 then last myWorkspaces else myWorkspaces !! i

-- }}}

-- {{{ Layouts

myLayout = smartBorders . avoidStruts . toggleLayouts Full $

    -- Layouts for workspaces
    onWorkspace (head myWorkspaces) myBrowser $
    onWorkspace (last myWorkspaces) myIM $

    -- Default
    Mirror tiled ||| tiled ||| Full ||| gimp ||| simplestFloat

  where
    tiled       = {- layoutHints $ -} ResizableTall 1 (3/100) (2/3) []

    myIM        = withIM (0.15) (Role "buddy_list") $ Grid ||| Mirror tiled ||| tiled ||| Full -- Pidgin buddy list
    myBrowser   = tabbedBottom shrinkText myTabbedTheme ||| Grid
    gimp        = withIM (0.15) (Role "gimp-toolbox") $
                  reflectHoriz $
                  withIM (0.21) (Role "gimp-dock") Full

-- }}}
