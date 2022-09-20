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
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.WindowSwallowing (swallowEventHook)

-- Layouts
import XMonad.Layout.MultiToggle (mkToggle, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing

-- Prompt
import XMonad.Prompt
import XMonad.Prompt.Shell (shellPrompt)

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Loggers
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
  ,("M-M1-f",                    sendMessage $ ToggleStruts)

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
myLayoutHook = avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ tall ||| wide ||| monocle
  where
    monocle = noBorders $ renamed [Replace "Monocle"] $ Full
    tall    = renamed [Replace "Tall"]    $ gaps $ Mirror $ Tall nmaster delta ratio
    wide    = renamed [Replace "Wide"]    $ gaps $ Tall nmaster delta ratio

    gaps    = spacingRaw False (Border 15 15 15 15) True (Border 15 15 15 15) True
    nmaster = 1     -- Default number of windows in the master pane
    delta   = 3/100 -- Default proportion of screen occupied by master pane
    ratio   = 1/2   -- Percentage of screen to increment by when resizing panes

--------------------------------------------------------------------------------
-- LOG HOOK
--------------------------------------------------------------------------------
--myLogHook = do
--  xmonadPropLog' "_XMONAD_LOG_1" =<< dynamicLogString myXmobarPP0
--  xmonadPropLog' "_XMONAD_LOG_2" =<< dynamicLogString myXmobarPP1

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
     , className =? "st-256color"     --> doShift (myWorkspaces !! 0)
     , className =? "zoom"            --> doShift (myWorkspaces !! 2)
     ]

--------------------------------------------------------------------------------
-- PROMPT
--------------------------------------------------------------------------------
myXPConfig :: XPConfig
myXPConfig = def
      { font                = "xft:Fira Code:regular:pixelsize=12"
      , bgColor             = background
      , fgColor             = white
      , bgHLight            = background
      , fgHLight            = red
      , borderColor         = background
      , promptBorderWidth   = myBorderWidth
      , position            = Top
      , height              = 20
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
                         "xmobar -x 0 $HOME/.config/xmobar/xmobarrc0 &"
                        ,"xmobar -x 1 $HOME/.config/xmobar/xmobarrc1 &"
                        ,"trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --height 20 --width 10 --transparent true --alpha 0 --tint 0xFF1b1f26 &"
                        ,"nitrogen --restore &"
                        ,"xsetroot -cursor_name left_ptr &"
                        ,"picom &"
                        , myTerminal ++ " &"
                        ,"setxkbmap -layout 'us' -variant 'dvorak' -option 'ctrl:swapcaps' &" 
                        ,"emacs --daemon &"
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
foreground = "#d8dee9"
background = "#1b1f26"

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

--------------------------------------------------------------------------------
-- WORKSPACES
--------------------------------------------------------------------------------
myWorkspaces = ["DEV","WEB","CHAT","GAME","AV","DLD","RAND"]

--------------------------------------------------------------------------------
-- XMOBAR
--------------------------------------------------------------------------------
myXmobarPP0 :: PP
myXmobarPP0 = def
    { ppSep             = makeBlue " : "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = makeRed . wrap "[" "]"
    , ppVisible         = makeOrange
    , ppHidden          = makeBlue . wrap "*" ""
    , ppHiddenNoWindows = makeBlue
    , ppUrgent          = makeRed . wrap "!" "!"
    , ppOrder           = \[ws, _, _, l, wins] -> [ws, l, wins]
    , ppExtras          = [logLayoutOnScreen 0, logTitlesOnScreen 0 formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (makeWhite     "[") (makeWhite     "]") . makeRed  . ppWindow
    formatUnfocused = wrap (makeDarkWhite "[") (makeDarkWhite "]") . makeBlue . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    makeBlue, makeDarkWhite, makeOrange, makeRed, makeWhite :: String -> String
    makeBlue      = xmobarColor blue      ""
    makeDarkWhite = xmobarColor darkWhite ""
    makeOrange    = xmobarColor orange    ""
    makeRed       = xmobarColor red       ""
    makeWhite     = xmobarColor white     ""
  
myXmobarPP1 :: PP
myXmobarPP1 = def
    { ppSep             = makeBlue " : "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = makeRed . wrap "[" "]"
    , ppVisible         = makeOrange
    , ppHidden          = makeBlue . wrap "*" ""
    , ppHiddenNoWindows = makeBlue
    , ppUrgent          = makeRed . wrap "!" "!"
    , ppOrder           = \[ws, _, _, l, wins] -> [ws, l, wins]
    , ppExtras          = [logLayoutOnScreen 1, logTitlesOnScreen 1 formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (makeWhite     "[") (makeWhite     "]") . makeRed  . ppWindow
    formatUnfocused = wrap (makeDarkWhite "[") (makeDarkWhite "]") . makeBlue . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    makeBlue, makeDarkWhite, makeOrange, makeRed, makeWhite :: String -> String
    makeBlue      = xmobarColor blue      ""
    makeDarkWhite = xmobarColor darkWhite ""
    makeOrange    = xmobarColor orange    ""
    makeRed       = xmobarColor red       ""
    makeWhite     = xmobarColor white     ""

--------------------------------------------------------------------------------
-- MAIN
--------------------------------------------------------------------------------
xmobar0 = statusBarPropTo "_XMONAD_LOG_1" "xmobar -x 0 $/HOME/.config/xmobar/xmobarrc0" (pure myXmobarPP0)
xmobar1 = statusBarPropTo "_XMONAD_LOG_2" "xmobar -x 1 $/HOME/.config/xmobar/xmobarrc1" (pure myXmobarPP1)
  
main :: IO ()
main = xmonad
     . ewmh
     . withSB (xmobar0 <> xmobar1)
     . docks
     $ myConfig

myConfig = def
  { handleEventHook    = myHandleEventHook
  , layoutHook         = myLayoutHook
--  , logHook            = myLogHook
  , manageHook         = myManageHook
  , startupHook        = myStartupHook
  , normalBorderColor  = myNormalColor
  , focusedBorderColor = cyan
  , borderWidth        = myBorderWidth
  , modMask            = mod4Mask
  , terminal           = myTerminal
  , workspaces         = myWorkspaces
  } `additionalKeysP` myKeys
