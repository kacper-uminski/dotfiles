--------------------------------------------------------------------------------
-- IMPORTS
--------------------------------------------------------------------------------

-- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (nextScreen, prevScreen)
import XMonad.Actions.WithAll (sinkAll)

-- Base
import XMonad
import Data.Monoid

-- Hooks
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.WindowSwallowing (swallowEventHook)

-- Layouts
import XMonad.Layout.MultiToggle (mkToggle, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed

-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)


--------------------------------------------------------------------------------
-- HANDLE EVENT HOOK
--------------------------------------------------------------------------------
myHandleEventHook :: Event -> X All
myHandleEventHook = swallowEventHook (className =? "Alacritty" <||> className =? "st-256color") (return True)

--------------------------------------------------------------------------------
-- KEYBINDINGS
--------------------------------------------------------------------------------
myKeys :: [(String,  X ())]
myKeys = [
-- Prompts
  ("M-r",                       shellPrompt myXPConfig)

-- Spawning and killing windows 
  ,("M-b",                       spawn "firefox")
  ,("M-e",                       spawn "emacs")
  ,("M-t",                       spawn myTerminal)
  ,("M-v",                       spawn $ myTerminal ++ " -e pulsemixer")
  ,("M-S-h",                     spawn $ myTerminal ++ " -e htop")
  ,("M-w",                       kill1)

-- Setting keyboard layouts
  ,("M-M1-p",                    spawn "setxkbmap -layout 'pl' -variant 'dvorak' -option 'ctrl:swapcaps'")
  ,("M-M1-s",                    spawn "setxkbmap -layout 'se' -variant 'dvorak' -option 'ctrl:swapcaps'")
  ,("M-M1-d",                    spawn "setxkbmap -layout 'us' -variant 'dvorak' -option 'ctrl:swapcaps'")

-- Setting Volume
  ,("<XF86AudioMute>",           spawn "pactl set-sink-mute 0 toggle")
  ,("<XF86AudioLowerVolume>",    spawn "pactl set-sink-volume 0 -1%")
  ,("<XF86AudioRaiseVolume>",    spawn "pactl set-sink-volume 0 +1%")

-- Screenshots
  ,("M-s",                       spawn "flameshot gui")

-- Window layouts
  ,("M-M1-m",                    sendMessage $ JumpToLayout "Monocle")
  ,("M-M1-t",                    sendMessage $ JumpToLayout "Tall")
  ,("M-M1-w",                    sendMessage $ JumpToLayout "Wide")

-- Workspaces
  ,("M-.",                       nextScreen)
  ,("M-,",                       prevScreen)
  ,("M-ö",                       nextScreen)
  ,("M-ä",                       prevScreen)

-- Xmonad actions
  ,("M-S-r",                     spawn "xmonad --restart")
  ,("M-S-f",                     sinkAll)
  ]

--------------------------------------------------------------------------------
-- LAYOUT HOOK
--------------------------------------------------------------------------------
myLayoutHook = mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ monocle ||| tall ||| wide
  where
    monocle = noBorders $ renamed [Replace "Monocle"] $ Full
    tall    = noBorders $ renamed [Replace "Tall"] $ Mirror $ Tall nmaster delta ratio
    wide    = noBorders $ renamed [Replace "Wide"] $ Tall nmaster delta ratio

    nmaster = 1     -- Default number of windows in the master pane
    delta   = 3/100 -- Default proportion of screen occupied by master pane
    ratio   = 1/2   -- Percentage of screen to increment by when resizing panes

--------------------------------------------------------------------------------
-- MANAGE HOOK
--------------------------------------------------------------------------------
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = insertPosition End Newer <+> composeAll
     [ className =? "Alacritty"       --> doShift (myWorkspaces !! 0)
     , className =? "Darktable"       --> doShift (myWorkspaces !! 4)
     , className =? "Emacs"           --> doShift (myWorkspaces !! 0)
     , className =? "Firefox"         --> doShift (myWorkspaces !! 1)
     , className =? "firefox"         --> doShift (myWorkspaces !! 1)
     , className =? "puddletag"       --> doShift (myWorkspaces !! 4)
     , className =? "qBittorrent"     --> doShift (myWorkspaces !! 5)
     , className =? "retroarch"       --> doShift (myWorkspaces !! 3)
     , className =? "Skype"           --> doShift (myWorkspaces !! 2)
     , className =? "Steam"           --> doShift (myWorkspaces !! 3)
     , className =? "TelegramDesktop" --> doShift (myWorkspaces !! 2)
     , className =? "Slack"           --> doShift (myWorkspaces !! 2)
     , className =? "st-256color"     --> doShift (myWorkspaces !! 0)
     , className =? "zoom"            --> doShift (myWorkspaces !! 2)
     ]

--------------------------------------------------------------------------------
-- PROMPT
--------------------------------------------------------------------------------
myXPConfig :: XPConfig
myXPConfig = def
      { font                = "xft:Plex Mono:regular:pixelsize=20"
      , bgColor             = background
      , fgColor             = white
      , bgHLight            = background
      , fgHLight            = red
      , borderColor         = background
      , promptBorderWidth   = myBorderWidth
      , position            = Top
      , height              = 40
      , historySize         = 256
      , historyFilter       = id
      , defaultText         = []
      , autoComplete        = Nothing
      , showCompletionOnTab = False
      , alwaysHighlight     = True
      , maxComplRows        = Just 1
      }

--------------------------------------------------------------------------------
-- STARTUP HOOK
--------------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
        mapM_ spawnOnce [
                        "xsetroot -cursor_name left_ptr &"
                        ,"picom &"
                        , myTerminal ++ " &"
                        ,"setxkbmap -layout 'us' -variant 'dvorak' -option 'ctrl:swapcaps' &" 
                        ,"unclutter -display :0.0 -idle 3 &"
                        ,"firefox &"
                        ,"flameshot &"
                        ,"telegram-desktop &"
                        ,"skypeforlinux &"
                        ]
        setWMName "LG3D"

--------------------------------------------------------------------------------
-- VARIABLES
--------------------------------------------------------------------------------

-- Colors

background, foreground :: String
foreground = "#ffffff"
background = "#000000"

black, red, orange, green, yellow, blue, magenta, cyan, white :: String
black = "#3b4252"
red = "#bf616a"
orange = "#d08770"
green = "#a3be8c"
yellow = "#ebcb8b"
blue = "#81a1c1"
magenta = "#b48ead"
cyan = "#88c0d0"
white = "#e5e9f0"

darkBlack, darkRed, darkGreen, darkYellow, darkBlue, darkMagenta, darkCyan, darkWhite :: String
darkBlack = "#373e4d"
darkRed = "#94545d"
darkGreen = "#809575"
darkYellow = "#b29e75"
darkBlue = "#68809a"
darkMagenta = "#8c738c"
darkCyan = "#6d96a5"
darkWhite = "#aeb3bb"

myNormalColor :: String
myNormalColor = "#1b1f26"

myBorderWidth :: Dimension
myBorderWidth = 2

myTerminal :: String
myTerminal = "alacritty"
--myTerminal = "st"
--myTerminal = "urxvtc"

--------------------------------------------------------------------------------
-- WORKSPACES
--------------------------------------------------------------------------------
myWorkspaces = ["DEV","WEB","CHAT","GAME","AV","DLD","RAND"]

main :: IO ()
main = xmonad
  . ewmhFullscreen
  . ewmh
  . docks
  $ def { handleEventHook    = myHandleEventHook
        , layoutHook         = myLayoutHook
        , manageHook         = myManageHook
        , startupHook        = myStartupHook
        , normalBorderColor  = myNormalColor
        , focusedBorderColor = cyan
        , borderWidth        = myBorderWidth
        , modMask            = mod4Mask
        , terminal           = myTerminal
        , workspaces         = myWorkspaces
        } `additionalKeysP` myKeys
