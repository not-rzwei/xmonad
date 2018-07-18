-- Libraries {{{
import Control.Monad
import XMonad

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe, safeSpawn)
import XMonad.Util.SpawnOnce (spawnOnce)

import XMonad.Layout
import XMonad.Layout.Gaps
import XMonad.Layout.IM
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.SimpleFloat
import XMonad.Layout.Square
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Renamed
import XMonad.Layout.TwoPane

import Graphics.X11.ExtraTypes.XF86
import System.IO
import XMonad.Actions.CycleWS
import XMonad.Actions.UpdatePointer
-- }}}

-- Main module {{{
main = do
  wsPanel     <- spawnPipe wsBar
  statsPanel  <- spawnPipe statsBar
  xmonad
    $ docks
    $ withUrgencyHook NoUrgencyHook
    $ defaultConfig
      -- Simple stuff
      { modMask             = modm
      , terminal            = term
      , focusFollowsMouse   = mouseFocus
      , borderWidth         = bdrSize
      , normalBorderColor   = bdrNormal
      , focusedBorderColor  = bdrFocus
      , workspaces          = workspaces'

      -- Lets hook up
      , handleEventHook     = eventHook
      , logHook             = logHook' wsPanel >> updatePointer(0.5, 0.5)(0, 0)
      , layoutHook          = layoutHook'
      , manageHook          = manageHook'
      , startupHook         = startupHook'
      } `additionalKeys` keyboard

---- Simple stuff
modm          = mod4Mask
term          = "urxvt"
mouseFocus    = False
workspaces'   = myWorkspaces
keyboard      = myKeys

----- Appearance
bdrSize       = 0
bdrNormal     = fgColor
bdrFocus      = bgColor
font          = "Misc Termsyn.Icons:size=13"
monitorSize   = 1366
monitor n     = show(round(monitorSize * n))
monitor' n    = round(monitorSize * n)

----- WHAT COLOR?
bgColor       = "#3d4142"
fgColor       = "#c3bfc0"
wsBgColor     = fgColor
wsFgColor     = bgColor
barBgColor    = bgColor
barFgColor    = fgColor
hintColor     = "#754e53"
layoutColor   = "#14272e"

---- Panel
leftBarSize   = monitor 0.7
rightBarSize  = monitor 0.3
leftBarPos    = "0"
rightBarPos   = leftBarSize
barHeight     = "26"

wsBar      =
  "dzen2 -dock -ta l      \
  \ -bg '" ++ barBgColor  ++ "' \
  \ -fg '" ++ barFgColor  ++ "' \
  \ -w  '" ++ leftBarSize ++ "' \
  \ -h  '" ++ barHeight   ++ "' \
  \ -x  '" ++ leftBarPos  ++ "' \
  \ -fn '" ++ font        ++ "' "

statsPanel      =
  "dzen2 -dock -ta r      \
  \ -bg '" ++ barBgColor   ++ "'\
  \ -fg '" ++ barFgColor   ++ "'\
  \ -w  '" ++ rightBarSize ++ "'\
  \ -h  '" ++ barHeight    ++ "'\
  \ -x  '" ++ rightBarPos  ++ "'\
  \ -fn '" ++ font         ++ "'"

statusConfig  = "conky -c ~/.xmonad/status.conf"
statsBar      = statusConfig ++ " | " ++ statsPanel

---- Hooks
eventHook     = fullscreenEventHook
layoutHook'   = myLayoutHook
logHook'      = myLogHook
manageHook'   = myManageHook
startupHook'  = myStartupHook
-- }}}

-- Log Hook {{{
myLogHook h =
  dynamicLogWithPP $
  dzenPP
    { ppOutput  = hPutStrLn h
    , ppCurrent = dzenColor (fg) (bg) . pad
    , ppVisible = pad
    , ppHidden  = pad
    , ppUrgent  = dzenColor (bg) (hint) . pad
    , ppSep     = ""
    , ppOrder   = \(ws:l:t:_) -> [l, ws]
    , ppLayout  = dzenColor (bg) (layoutBg) . pad . pad .
        ( \t -> case t of
          "Tall" -> "þ"
          "Mirror Tall" -> "ü"
          "Full" -> "ÿ"
          _ -> t
        )
    }
  where
    bg = wsBgColor
    fg = wsFgColor
    hint = hintColor
    layoutBg = layoutColor
-- }}}

-- Workspaces {{{
myWorkspaces = ws $ ["TERM", "INET", "DEV", "ENT", "PLAY", "TOOL"]
  where
    ws l =
      [ "^ca(1,xdotool key super+" ++ show n ++ ")  " ++ ws ++ "  ^ca()"
      | (i, ws) <- zip [1 ..] l
      , let n = i
      ]
-- }}}

-- Layout Hook {{{
myLayoutHook =
  avoidStruts
  $ mkToggle (NOBORDERS ?? FULL ?? EOT)
  $ onWorkspace (w !! 0) termLayout
  $ onWorkspace (w !! 1) webLayout
  $ standardLayout
  where
    w = workspaces'
    termLayout =
      renamed [CutWordsLeft 1] $
      gaps [(L,30), (U,30), (R,30), (D,30)] $
      withIM (6/11) (ClassName "URxvt") $
      standardLayout
    webLayout = Full ||| Tall (1) (3/100) (1/2)
    standardLayout =
      smartBorders $
      renamed [CutWordsLeft 2] $
      smartSpacingWithEdge 8 $ layoutHook defaultConfig
-- }}}

-- Manage Hook {{{
myManageHook =
  composeAll . concat $
  [ [ className =? c --> doShift (w !! 1) | c <- inetApp ]
  , [ className =? c --> doShift (w !! 2) | c <- devApp ]
  , [ className =? c --> doShift (w !! 3) | c <- entApp ]
  , [ className =? c --> doShift (w !! 4) | c <- playApp ]
  , [ className =? c --> doFloat          | c <- floatingApp ]
  , [ className =? c --> doIgnore         | c <- ignoreApp ]
  , [ isDialog       --> doCenterFloat ]
  , [ isRole         --> doCenterFloat ]
  , [ manageDocks ]
  , [ manageHook def ]
  ]
  where
    w = workspaces'
    isRole = stringProperty "WM_WINDOW_ROLE" =? "pop-up"
    inetApp = ["Chromium"]
    devApp =
      [ "SecureCRT", "GNS3", "VirtualBox Manager"
      , "VirtualBox Machine", "jetbrains-studio"
      , "Code", "oni"
      ]
    entApp = ["MPlayer", "smplayer", "mpv", "Gimp"]
    playApp = ["player", "Genymotion"]
    floatingApp = ["SecureCRT", "TeamViewer", "Xmessage"]
    ignoreApp = ["desktop", "desktop_window", "stalonetray", "trayer"]
-- }}}


-- Startup Hook {{{
myStartupHook = do
  spawnOnce "feh --bg-fill ~/.xmonad/background.png"
  spawnOnce "xsetroot -cursor_name left_ptr"
  spawnOnce "setxkbmap -option 'altwin:swap_alt_win'"
  spawnOnce "compton --config /dev/null -bGC \
            \ --focus-exclude \"class_g = 'Dmenu'\" \
            \ --inactive-dim 0.5 "
  spawnOnce "xclip"
-- }}}


-- Keymapping {{{
-- /usr/include/X11/keysymdef.h
myKeys =
  [ ((m, xK_b), spawn "chromium")
  , ((m, xK_p), spawn dmenu)
  , ((m, xK_s), sendMessage ToggleStruts)
  , ((m, xK_BackSpace), focusUrgent)
  , ((m, xK_equal), toggleWS)
  , ((m, xK_grave), toggleWS)
  , ((m, xK_Caps_Lock), sendMessage $ Toggle FULL)
  , ((0, xF86XK_AudioLowerVolume), spawn "ponymix decrease 10")
  , ((0, xF86XK_AudioRaiseVolume), spawn "ponymix increase 10")
  , ((0, xF86XK_AudioMute), spawn "ponymix toggle")
  , ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -10")
  , ((0, xF86XK_MonBrightnessUp), spawn "xbacklight +10")
  , ((0, xK_Print), spawn "maim -s | xclip -selection clipboard -t image/png")
  , ((s, xK_Print), spawn "maim | xclip -selection clipboard -t image/png")
  ]
  where
    m = modm
    s = shiftMask
    c = controlMask
    dmenu =
      "dmenu_run -i \
      \ -fn '" ++ fn ++ "' \
      \ -w '" ++ w ++ "' \
      \ -h '" ++ h ++ "' \
      \ -x '" ++ x ++ "' \
      \ -nb '" ++ nb ++ "' \
      \ -sb '" ++ sb ++ "'"
      where
        w = monitor 0.3
        x = show(monitor'(0.7) - 70)
        h = "26"
        fn = font
        nb = bgColor
        sb = layoutColor
-- }}}
