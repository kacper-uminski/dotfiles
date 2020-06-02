------------------------------------------------------------------------
---IMPORTS
------------------------------------------------------------------------
-- Actions
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.SinkAll
import XMonad.Actions.SpawnOn

-- Base
import XMonad
import System.IO

-- Hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

-- Layouts
import XMonad.Layout.Gaps
import XMonad.Layout.Grid
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.Spacing

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run
import XMonad.Util.SpawnOnce


------------------------------------------------------------------------
---CONFIG
------------------------------------------------------------------------
-- Variables
myBorderWidth   = 2         -- Sets border width for windows
myFont          = "xft:BlexMono Nerd Font Complete:regular:pixelsize=12"
myModMask       = mod4Mask  -- Sets modkey to super/windows key
myTerminal      = "alacritty"      -- Sets default terminal
myTextEditor    = "nvim"     -- Sets default text editor

-- Main config
main = do

-- Spawn xmobar
    xmproc <- spawnPipe "/usr/bin/xmobar /home/kacper/.config/xmobar/xmobarrc"

-- Main config
    xmonad $ docks defaultConfig
        { manageHook = insertPosition End Newer <+> manageSpawn <+> manageDocks <+> manageHook defaultConfig
        , logHook = dynamicLogWithPP xmobarPP
                        { ppOutput =          \x -> hPutStrLn xmproc x
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
myKeys = 
-- Xmonad actions
        [ ("M-S-r",  spawn "xmonad --restart")
        , ("M-S-f",  sinkAll)

-- Spawning and killing windows.
        , ("M-b",    spawnOn "WEB"  "chromium")
        , ("M-e",    spawnOn "DEV"  "emacs")
        , ("M-t",    spawnOn "DEV"   myTerminal)
        , ("M-m",    spawnOn "AUD"  (myTerminal ++ " -e ncspot"))
        , ("M-v",    spawn          (myTerminal ++ " -e pulsemixer"))
        , ("M-r",    spawn   "dmenu_run -l 0 -h 10")
        , ("M-w",    kill1)                           -- Kills selected window

-- Setting keyboard layouts.
        , ("M-M1-p", spawn "setxkbmap -layout 'pl' -variant 'dvorak' -option 'ctrl:swapcaps'")
        , ("M-M1-s", spawn "setxkbmap -layout 'se' -variant 'dvorak' -option 'ctrl:swapcaps'")
        , ("M-M1-d", spawn "setxkbmap -layout 'us' -variant 'dvorak' -option 'ctrl:swapcaps'")

-- Window layouts.
        , ("M-M1-l", sendMessage NextLayout)
        , ("M-M1-f", sendMessage ToggleStruts)
        , ("M-M1-b", sendMessage $ Toggle NOBORDERS)
        ]


------------------------------------------------------------------------
---LAYOUTS
------------------------------------------------------------------------
myLayoutHook = avoidStruts $ mkToggle (NBFULL ?? NOBORDERS ?? EOT) $ myDefaultLayout

    where
        myDefaultLayout = tall ||| noBorders monocle ||| grid 

grid =      renamed [Replace "Grid"]    $ spacing 15 $ gaps [(U,15), (D,15), (L,15), (R,15)] $ Grid
monocle =   renamed [Replace "Monocle"] $ Full
tall =      renamed [Replace "Tall"]    $ spacing 15 $ gaps [(U,15), (D,15), (L,15), (R,15)] $ Tall 1 (3/100) (1/2)


------------------------------------------------------------------------
---AUTOSTART
------------------------------------------------------------------------
myStartupHook = do
          spawnOnce (myTerminal ++ "&")
          spawnOnce "emacs --daemon &" 
          spawnOnce "feh --bg-fill /home/kacper/Pictures/Wallpapers/Backdrops/Dazzled-Horizon.png &"
          spawnOnce "picom &"
          spawnOnce "unclutter -display :0.0 -idle 3 &"
          spawnOnce "setxkbmap -layout 'us' -variant 'dvorak' -option 'ctrl:swapcaps'" 
          setWMName "LG3D"

------------------------------------------------------------------------
---WORKSPACES
------------------------------------------------------------------------
myWorkspaces = ["DEV","WEB","CHAT","GAME","AUD","RAND"] -- you can customize the names of the default workspaces by changing the list

