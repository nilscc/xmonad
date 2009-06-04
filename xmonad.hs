----------------------------------------------------------------------
--                                                                  --
-- XMonad Settings                                                  --
--                                                                  --
--                                                                  --
-- XMonad version 0.8.1-darcs                                       --
--                                                                  --
-- by Nils                                                          --
--                                                                  --
-- For questions, write an email to:                                --
-- mail (at) n-sch . de                                             --
-- Or visit the #xmonad channel at irc.freenode.net                 --
--                                                                  --
--                                                                  --
-- Additional files:                                                --
-- http://www.n-sch.de/xmonad/icons.tar                             --
-- http://www.n-sch.de/xmonad/.conkytoprc                           --
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
-- mod + F1             - Start urxvtc                              --
-- mod + F2             - Start dmenu                               --
-- mod + F3             - Start thunar                              --
-- mod + F4             - Start opera                               --
-- mod + F5             - Start irssi in urxvtc                     --
-- mod + F6             - Start pidgin on last workspace            --
-- mod + F7             - Start mutt in urxvtc                      --
-- mod + F8             - Start ncmpcpp in urxvtc                   --
--                                                                  --
-- mod + j/k            - Go up/down in window list                 --
-- mod + h/l            - Adjust width/height of master             --
-- mod + ,/.            - Adjust numbers of master windows          --
-- mod + shift + j/k    - Adjust width/height of slaves             --
-- mod + space          - Next layout                               --
-- mod + shift + space  - Reset layout                              --
--                                                                  --
-- mod + 1-0            - View workspace x                          --
-- mod + shift + 1-0    - Move current window to workspace x        --
-- mod + y              - Previous workspace                        --
-- mod + x              - Next workspace                            --
-- mod + mousewheelUP   - Previous workspace                        --
-- mod + mousewheelDOWN - Next workspace                            --
-- mod + ^              - View last workspace ("com")               --
-- mod + tab            - Cycle through recent (hidden) workspaces  --
--                        tab - next workspace                      --
--                        esc - previous workspace                  --
--                        release mod to exit cycle mode            --
-- mod + a              - Cycle through visible screens             --
--                        a - next screen                           --
--                        s - previous screen                       --
--                        release mod to exit cycle mode            --
-- mod + s              - Swap current screen with visible ws       --
--                        s - next workspace                        --
--                        d - previous workspace                    --
--                        release mod to exit cycle mode            --
--                                                                  --
-- mod + q              - View urgent workspace                     --
-- mod + w              - Focus master window                       --
-- mod + w              - Swap current window with master           --
--                                                                  --
-- mod + f              - Toggle fullscreen                         --
-- mod + leftclick      - Float window                              --
-- mod + rightclick     - Adjust size of floated window             --
-- mod + t              - Push window back into tiling              --
-- mod + shift + t      - Push all windows back into tiling         --
--                                                                  --
-- Mediakeys            - Control MPD                               --
--                                                                  --
-- mod + shift + w      - Kill current window                       --
-- mod + shift + n      - Refresh current window                    --
-- mod + shift + r      - Restart XMonad                            --
--                                                                  --
-- mod + control + backspace - Exit XMonad                          --
--                                                                  --
-- }}}                                                              --
----------------------------------------------------------------------

-- {{{ imports

-- Core
import XMonad

import Data.Monoid
import System.IO
import System.Exit

import Control.Monad (liftM2)

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- usefull stuff
import Graphics.X11.ExtraTypes.XF86 

-- Actions
import XMonad.Actions.WindowGo
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.SinkAll

-- Utils
import XMonad.Util.Run (spawnPipe)

-- Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.FadeInactive

import XMonad.Hooks.ServerMode
import XMonad.Hooks.EventHook

-- Layouts
import XMonad.Layout.Reflect
import XMonad.Layout.PerWorkspace
import XMonad.Layout.LayoutHints
import XMonad.Layout.ToggleLayouts
--import XMonad.Layout.SimpleDecoration

import XMonad.Layout.ResizableTile
import XMonad.Layout.Grid
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns

import XMonad.Layout.IM
import Data.Ratio ((%))

-- }}}

mpd_host = "192.168.0.31"
--mpd_host = "localhost"

-- {{{ Run XMonad

main = do
    dzen    <- spawnPipe myStatusBar
    conky   <- spawnPipe myConkyBar
    xmonad $ withUrgencyHook NoUrgencyHook defaultConfig {

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
        , logHook            = do
            dynamicLogWithPP (myLogHook dzen)
            fadeInactiveLogHook 0xdddddddd
        , startupHook        = myStartupHook

    }

-- }}}

-- {{{ Settings

myWorkspaces            :: [String]
myWorkspaces            = clickable . (map dzenEscape) $ nWorkspaces 9 ["web", "irc", "com"]

  where nWorkspaces n []= map show [1 .. n]
        nWorkspaces n l = init l ++ map show [length l .. n] ++ [last l]
        clickable l     = [ "^ca(1,xdotool key super+" ++ show (n) ++ ")" ++ ws ++ "^ca()" |
                            (i,ws) <- zip [1..] l,
                            let n = if i == 10 then 0 else i ]


myTerminal              = "urxvtc"

myModMask               = mod4Mask

myFocusFollowsMouse     = False

myBorderWidth           = 2
myNormalBorderColor     = "#000000"
myFocusedBorderColor    = "#555555"

-- }}}

-- {{{ Log hook

-- Statusbar with workspaces, layout and title
myStatusBar = "dzen2 -x 0   -y 0 -h 18 -ta l -fg '" ++ myDzenFGColor ++ 
              "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "' -w 720"

-- Conky with MPD_HOST - veeeeeeery dirty hack
myConkyBar  = "export MPD_HOST=" ++ mpd_host ++ " && echo \"mpd_host $MPD_HOST\" | "
              ++ "cat - ~/.conkytoprc > /tmp/.conkytoprc && conky -c /tmp/.conkytoprc | " ++
              "dzen2 -x 720 -y 0 -h 18 -ta r -fg '" ++ myDzenFGColor ++ 
              "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont ++ "' -w 720"


-- Colors, font and iconpath definitions:
myFont = "-*-snap-normal-r-normal-*-10-*-*-*-*-*-*-*" -- change, if you dont have artwiz fonts
myIconDir = "/home/nils/.dzen" -- get icons at http://www.n-sch.de/xmonad/icons.tar
myDzenFGColor = "#555555"
myDzenBGColor = ""
myNormalFGColor = "#77e000"
myNormalBGColor = "#000000"
myFocusedFGColor = "#f0f0f0"
myFocusedBGColor = "#333333"
myUrgentFGColor = "#0099ff"
myUrgentBGColor = "#991133"
mySeperatorColor = "#555555"


-- LogHook-Settings
myLogHook h = defaultPP
    { ppCurrent	    = dzenColor myNormalFGColor myNormalBGColor . pad . ("^i(.dzen/corner.xbm)" ++) -- current workspace
    , ppVisible	    = dzenColor "lightgreen" ""                 . pad                               -- visible workspaces on other screens
    , ppHidden	    = dzenColor "white" ""                      . pad . ("^i(.dzen/corner.xbm)" ++) -- hidden workspaces with apps
    , ppHiddenNoWindows     = dzenColor "#444444"  ""           . pad                               -- empty workspaces
    , ppUrgent	    = dzenColor "" myUrgentBGColor                                                  -- urgent workspaces
    , ppTitle       = dzenColor myNormalFGColor ""              . pad . dzenEscape                  -- title of selected window
    , ppWsSep       = ""                                                                            -- workspace seperator
    , ppSep         = dzenEscape "|"                                                                -- workspace/layout/title seperator

    -- Layout icons
    , ppLayout      = dzenColor myNormalFGColor "" .
        (\ x -> case x of
                     "Hinted ResizableTall"             -> pad "^i(.dzen/layout-tall-right.xbm)"
                     "Mirror Hinted ResizableTall"      -> pad "^i(.dzen/layout-mirror-bottom.xbm)"
                     "Full"                             -> pad "^i(.dzen/layout-full.xbm)"
                     "IM Grid"                          -> pad "^i(.dzen/layout-withim-left1.xbm)"
                     "IM Hinted ResizableTall"          -> pad "^i(.dzen/layout-withim-left2.xbm)"
                     "IM Mirror Hinted ResizableTall"   -> pad "^i(.dzen/layout-withim-left3.xbm)"
                     "IM Full"                          -> pad "^i(.dzen/layout-withim-left4.xbm)"
                     "IM ReflectX IM Full"              -> pad "^i(.dzen/layout-gimp.xbm)"
                     _                                  -> pad x
        )

	, ppOutput      = hPutStrLn h
    }

-- }}}

-- {{{ Other hooks

-- Event hook
myEventHook = const . return $ All True

-- Startup hook
myStartupHook = setWMName "LG3D"

-- Manage hook
myManageHook = composeAll . concat $

    -- Float apps
    [ [ className =? c                  --> doCenterFloat | c <- myCFloats    ]
    , [ resource  =? r                  --> doCenterFloat | r <- myRFloats    ]
    , [ title     =? t                  --> doCenterFloat | t <- myTFloats    ]

    -- "Real" fullscreen
    , [ isFullscreen                    --> doFullFloat ]

    -- Workspaces
    -- Be carefull with (!!) - if n is too big xmonad crashes!
    , [ className =? "Firefox"          --> doF (liftM2 (.) W.view W.shift $ myWorkspaces !! 0) ]
    , [ className =? "Opera"            --> doF (liftM2 (.) W.view W.shift $ myWorkspaces !! 0) ]
    , [ className =? "Xchat"            --> doF (liftM2 (.) W.view W.shift $ myWorkspaces !! 1) ]
    , [ resource  =? "irssi"            --> doF (liftM2 (.) W.view W.shift $ myWorkspaces !! 1) ]
    , [ className =? "Pidgin"           --> doF (W.shift $ last myWorkspaces) ]
    ]

  where myCFloats = [{- "Gnome-mplayer", "gpicview", -} "Wine", "Switch2"]
        myRFloats = ["Dialog", "Download"]
        myTFloats = ["Schriftart auswählen"]
        qa /=? a  = fmap not (qa =? a)


-- }}}

-- {{{ Key & Mouse

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $

    -- General moving & stuff
    [ ((modMask .|. shiftMask, xK_w     ), kill) -- close focused window 

    , ((modMask              , xK_space ), sendMessage NextLayout) -- Rotate through the available layout algorithms
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) --  Reset the layouts on the current workspace to default

    , ((modMask              , xK_n     ), refresh) -- Resize viewed windows to the correct size

    , ((modMask              , xK_j     ), windows W.focusDown) -- Move focus to the next window
    , ((modMask              , xK_k     ), windows W.focusUp  ) -- Move focus to the previous window
    , ((modMask              , xK_q     ), focusUrgent        ) -- Focus urgent windows

    , ((modMask              , xK_w     ), windows W.focusMaster    ) -- Move focus to the master window
    , ((modMask              , xK_e     ), windows W.swapMaster     ) -- Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown       ) -- Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp         ) -- Swap the focused window with the previous window

    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))      -- Increment the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))   -- Deincrement the number of windows in the master area

    , ((modMask              , xK_h     ), sendMessage Shrink) -- Shrink the master area
    , ((modMask              , xK_l     ), sendMessage Expand) -- Expand the master area
    , ((modMask .|. controlMask, xK_j   ), sendMessage MirrorShrink) -- Shrink slaves
    , ((modMask .|. controlMask, xK_k   ), sendMessage MirrorExpand) -- Expand slaves

    , ((modMask              , xK_f     ), sendMessage ToggleStruts >> sendMessage ToggleLayout) -- Toggle fullscreen
    , ((modMask              , xK_t     ), withFocused $ windows . W.sink) -- Push window back into tiling
    , ((modMask .|. shiftMask, xK_t     ), sinkAll) -- Push all floating windows back into tiling

    , ((modMask .|. shiftMask, xK_r     ), spawn "killall conky" >> restart "xmonad" True) -- Restart xmonad, quit conky first

    , ((modMask .|. controlMask, xK_BackSpace), io $ exitWith ExitSuccess) -- quit xmonad

    ]
    ++

    -- Applications to run
    [ ((modMask              , xK_F1     ), spawn myTerminal)
    , ((modMask              , xK_F2     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"") -- launch dmenu
    , ((modMask              , xK_F3     ), spawn "thunar")
    , ((modMask              , xK_F4     ), runOrRaise "opera" (className =? "Opera"))
    --, ((modMask              , xK_F4     ), runOrRaise "firefox" (className =? "Firefox"))
    --, ((modMask              , xK_F5     ), runOrRaise "xchat" (className =? "Xchat"))
    , ((modMask              , xK_F5     ), runOrRaise (myTerminal ++ " -name irssi -e irssi") (resource =? "irssi"))
    , ((modMask              , xK_F6     ), runOrRaise "pidgin"  (className =? "Pidgin"))
    , ((modMask              , xK_F7     ), runOrRaise (myTerminal ++ " -name mutt -e mutt ") (resource =? "mutt"))
    , ((modMask .|. shiftMask, xK_F7     ), runOrRaise "thunderbird3" (className =? "Thunderbird"))
    , ((modMask              , xK_F8     ), runOrRaise ("export MPD_HOST=" ++ mpd_host ++ " && " ++ myTerminal ++ " -name ncmpcpp -e ncmpcpp") (resource =? "ncmpcpp"))
    , ((modMask .|. shiftMask, xK_F8     ), runOrRaise "gmpc" (className =? "Gmpc"))
    ]
    ++

    -- Multimedia keys
    -- Set according xmodmap!
    -- [ ((0                   , xF86XK_AudioMute),        spawn "amixer set Master toggle")
    -- , ((0                   , xF86XK_AudioRaiseVolume), spawn "amixer set Master 1+")
    -- , ((0                   , xF86XK_AudioLowerVolume), spawn "amixer set Master 1-")
    [ ((0                   , xF86XK_AudioMute),        spawn "/home/nils/.myscripts/ossvol -t")
    , ((0                   , xF86XK_AudioRaiseVolume), spawn "/home/nils/.myscripts/ossvol -i 1")
    , ((0                   , xF86XK_AudioLowerVolume), spawn "/home/nils/.myscripts/ossvol -d 1")
    , ((0                   , xF86XK_AudioPlay),        spawn $ "export MPD_HOST=" ++ mpd_host ++ " && mpc toggle")
    , ((0                   , xF86XK_AudioStop),        spawn $ "export MPD_HOST=" ++ mpd_host ++ " && mpc stop")
    , ((0                   , xF86XK_AudioPrev),        spawn $ "export MPD_HOST=" ++ mpd_host ++ " && mpc prev")
    , ((0                   , xF86XK_AudioNext),        spawn $ "export MPD_HOST=" ++ mpd_host ++ " && mpc next")
    ]
    ++

    -- mod-[1..9]       , Switch to workspace N
    -- mod-shift-[1..9] , Move client to workspace N, then switch to it
    [ ((m .|. modMask, k), windows (f i) {- >> spawnMaybe i -} )
         | (i, k) <- zip (workspaces conf) ([xK_1 .. xK_9] ++ [xK_0])
         , (f, m) <- [(W.greedyView, 0), (liftM2 (.) W.greedyView W.shift, shiftMask)] ]
    ++
    -- Move to the last workspace (my IM ws)
    [ ((modMask              , xK_asciicircum), windows $ W.greedyView (last myWorkspaces))


    -- Cycle recent (not visible) workspaces, tab is next, escape previous in history
    , let options w     = map (W.greedyView `flip` w)         (hiddenTags w)
      in ((modMask           , xK_Tab        ), cycleWindowSets options [xK_Super_L] xK_Tab xK_Escape)
    -- Swap visible workspaces on current screen, s is next, d previous
    , let options w     = map (W.greedyView `flip` w)   (visibleTags w)
      in ((modMask           , xK_s          ), cycleWindowSets options [xK_Super_L] xK_s xK_d)

    -- Swap visible workspaces on current screen, s is next, d previous
    -- , let options w     = map (flip $ W.view . W.greedyView $ w)   (visibleTags w)
      -- in ((modMask.|.shiftMask,xK_s          ), cycleWindowSets options [xK_Super_L] xK_s xK_d)

    -- Cycle through visible screens, a is next, s previous
    , let options w     = map (W.view `flip` w)         (visibleTags w)
      in ((modMask           , xK_a          ), cycleWindowSets options [xK_Super_L] xK_a xK_s)

    , ((modMask              , xK_Right      ), nextWS) -- Next workspace
    , ((modMask              , xK_Left       ), prevWS) -- Previous workspace
    ]

  where hiddenTags w  = map W.tag $ W.hidden w ++ [W.workspace . W.current $ w]
        visibleTags w = map (W.tag . W.workspace) $ W.visible w ++ [W.current w]

        {-
        -- Run application when switching to workspace N
        -- Carefull with the (!!) operator
        spawnMaybe ws
            | ws == (workspaces conf !! 4) = spawn "urxvt"
            | otherwise = return ()
        -}


myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $

    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)) -- Set the window to floating mode and move by dragging
    , ((modMask, button2), (\w -> focus w >> windows W.shiftMaster)) -- Raise the window to the top of the stack
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)) -- Set the window to floating mode and resize by dragging
    , ((modMask, button4), (\_ -> prevWS)) -- previous Workspace
    , ((modMask, button5), (\_ -> nextWS)) -- next Workspace

    ]

-- }}}

-- {{{ Layouts

myLayout = --simpleDeco shrinkText defaultTheme .
    smartBorders . avoidStruts .
    toggleLayouts Full $

    -- Layouts for workspaces
    onWorkspace (myWorkspaces !! 1) (Full ||| Mirror tiled ||| tiled)   $
    onWorkspace (last myWorkspaces) myIM                                $

    -- Default
    tiled ||| Mirror tiled ||| Full ||| gimp

  where
    tiled       = layoutHints $ ResizableTall 1 (3/100) (2/3) []

    myIM        = withIM (0.15) (Role "buddy_list") $ Grid ||| tiled ||| Mirror tiled ||| Full -- Pidgin buddy list
    gimp        = withIM (0.15) (Role "gimp-toolbox") $
                  reflectHoriz $
                  withIM (0.21) (Role "gimp-dock") Full

-- }}}
