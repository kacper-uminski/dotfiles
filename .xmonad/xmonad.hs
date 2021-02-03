------------------------------------------------------------------------
---IMPORTS
------------------------------------------------------------------------
-- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.CycleWS (nextScreen, prevScreen)
import XMonad.Actions.SinkAll

-- Base
import XMonad
import System.IO (hPutStrLn)

-- Data
import Data.Monoid

-- Hooks
import XMonad.Hooks.DynamicLog (dynamicLogWithPP, wrap, xmobarColor, xmobarPP, PP(..))
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

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
import XMonad.Util.Run (spawnPipe)
import XMonad.Util.SpawnOnce (spawnOnce)


------------------------------------------------------------------------
---CONFIG
------------------------------------------------------------------------
-- Variables
myBorderWidth :: Dimension
myBorderWidth = 2

myBrowser :: String
myBrowser = "firefox"

myFont :: String
myFont = "xft:BlexMono Nerd Font Complete:regular:pixelsize=12"

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "st"

myTextEditor :: String
myTextEditor = "nvim"

-- Main Function
main :: IO ()
main = do

-- Spawn xmobar
    xmproc0 <- spawnPipe "xmobar -x 0 $HOME/.config/xmobar/xmobarrc"
    xmproc1 <- spawnPipe "xmobar -x 1 $HOME/.config/xmobar/xmobarrc"

-- Main config
    xmonad $ docks def
        { manageHook = insertPosition End Newer <+> manageDocks <+> myManageHook
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput =          \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x
                        , ppCurrent =         xmobarColor "#63f2f1" "" . wrap "[" "]"    -- Current workspace in xmobar
                        , ppVisible =         xmobarColor "#65b2ff" ""                   -- Visible but not current workspace
                        , ppHidden =          xmobarColor "#65b2ff" "" . wrap "*" ""     -- Hidden workspaces in xmobar
                        , ppHiddenNoWindows = xmobarColor "#906cff" ""                   -- Hidden workspaces (no windows)
                        , ppSep =             " : "                                      -- Separators in xmobar
                        , ppUrgent =          xmobarColor "#ff5458" "" . wrap "!" "!"    -- Urgent workspace
                        , ppOrder  =          \(ws:l:t:ex) -> [ws,l]++ex
                        }
        , normalBorderColor  = "#1b182c"
        , focusedBorderColor = "#906cff"
        , borderWidth        = myBorderWidth
        , layoutHook         = myLayoutHook
        , modMask            = myModMask
        , startupHook        = myStartupHook
        , terminal           = myTerminal
        , workspaces         = myWorkspaces
        } `additionalKeysP` myKeys


------------------------------------------------------------------------
---KEYBINDINGS
------------------------------------------------------------------------
myKeys :: [(String, X ())]
myKeys = 
-- Prompts
        [ ("M-r",    shellPrompt myXPConfig)

-- Spawning and killing windows
        , ("M-b",    spawn  myBrowser)
        , ("M-t",    spawn   myTerminal)
        , ("M-v",    spawn  (myTerminal ++ " -e pulsemixer"))
        , ("M-w",    kill1)                                    -- Kills selected window

-- Setting keyboard layouts
        , ("M-M1-p", spawn "setxkbmap -layout 'pl' -variant 'dvorak' -option 'ctrl:swapcaps'")
        , ("M-M1-s", spawn "setxkbmap -layout 'se' -variant 'dvorak' -option 'ctrl:swapcaps'")
        , ("M-M1-d", spawn "setxkbmap -layout 'us' -variant 'dvorak' -option 'ctrl:swapcaps'")

-- Window layouts
        , ("M-M1-l", sendMessage NextLayout)
        , ("M-M1-f", sendMessage ToggleStruts)

-- Workspaces
        , ("M-.",    nextScreen)
        , ("M-,",    prevScreen)
-- Xmonad actions
        , ("M-S-r",  spawn "xmonad --restart")
        , ("M-S-f",  sinkAll)
        ]


------------------------------------------------------------------------
---LAYOUTS
------------------------------------------------------------------------
myLayoutHook = avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ tall ||| noBorders monocle
    where   monocle =   renamed [Replace "Monocle"] $ Full
            tall =      renamed [Replace "Tall"]    $ spacingRaw False (Border 15 15 15 15) True (Border 15 15 15 15) True $ Tall 1 (3/100) (1/2)


------------------------------------------------------------------------
---WORKSPACE PINNING
------------------------------------------------------------------------
myManageHook :: XMonad.Query (Data.Monoid.Endo WindowSet)
myManageHook = composeAll
     [ className =? "Alacritty"                 --> doShift (myWorkspaces !! 0)
     , className =? "Firefox"                   --> doShift (myWorkspaces !! 1)
     , className =? "Hiro"                      --> doShift (myWorkspaces !! 3)
     , className =? "Microsoft Teams - Preview" --> doShift (myWorkspaces !! 2)
     , className =? "Robocraft.x86_64"          --> doShift (myWorkspaces !! 3)
     , className =? "st-256color"               --> doShift (myWorkspaces !! 0)
     , className =? "Steam"                     --> doShift (myWorkspaces !! 3)
     , className =? "TelegramDesktop"           --> doShift (myWorkspaces !! 2)
     ]


------------------------------------------------------------------------
---AUTOSTART
------------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
        mapM_ spawnOnce [(myTerminal ++ " &")
                        ,"feh --bg-fill /home/kacper/pictures/wallpapers/backdrops/dazzled-horizon.png &"
                        ,"picom &"
                        ,"setxkbmap -layout 'us' -variant 'dvorak' -option 'ctrl:swapcaps'" 
                        ,"unclutter -display :0.0 -idle 3 &"
                        ,"xsetroot -cursor_name left_ptr"
                        ]
        setWMName "XMonad"


-------------------------------------------------------------------------
---WORKSPACES
------------------------------------------------------------------------
myWorkspaces = ["DEV","WEB","CHAT","GAME","AUD","RAND"]


------------------------------------------------------------------------
---PROMPT
------------------------------------------------------------------------
myXPConfig :: XPConfig
myXPConfig = def
      { font                = myFont
      , bgColor             = "#1b182c"
      , fgColor             = "#cbe3e7"
      , bgHLight            = "#1b182c"
      , fgHLight            = "#906cff"
      , borderColor         = "#906cff"
      , promptBorderWidth   = 2
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
