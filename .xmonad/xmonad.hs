------------------------------------------------------------------------
---IMPORTS
------------------------------------------------------------------------
-- Actions
import XMonad.Actions.CopyWindow (kill1)

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
import XMonad.Layout.Renamed (renamed, Rename(CutWordsLeft, Replace))
import XMonad.Layout.Spacing

-- Utilities
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (safeSpawn, unsafeSpawn, runInTerm, spawnPipe)
import XMonad.Util.SpawnOnce


------------------------------------------------------------------------
---CONFIG
------------------------------------------------------------------------
-- Variables
myBorderWidth   = 2         -- Sets border width for windows
myFont          = "xft:Blex Mono Nerd Font:regular:pixelsize=12"
myModMask       = mod4Mask  -- Sets modkey to super/windows key
myTerminal      = "alacritty"      -- Sets default terminal
myTextEditor    = "nvim"     -- Sets default text editor

-- Main config
main = do

-- Spawn xmobar
    xmproc <- spawnPipe "/usr/bin/xmobar /home/kacper/.config/xmobar/xmobarrc"

-- Main config
    xmonad $ docks defaultConfig
        { manageHook = insertPosition End Newer <+> manageDocks <+> manageHook defaultConfig
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
        , modMask            = myModMask
        , terminal           = myTerminal
        , startupHook        = myStartupHook
        , layoutHook         = myLayoutHook
        } `additionalKeysP` myKeys


------------------------------------------------------------------------
---KEYBINDINGS
------------------------------------------------------------------------
myKeys = 
        [ ("M-b",   spawn "brave")
        , ("M-e",   spawn "emacs")
        , ("M-f",   sendMessage ToggleStruts)
        , ("M-r",   spawn "dmenu_run -l 10")
        , ("M-t",   spawn myTerminal)
        , ("M-w",   kill1) -- Kills selected window
        , ("M-S-p", spawn "setxkbmap -layout 'pl' -variant 'dvorak' -option 'ctrl:swapcaps'")
        , ("M-S-s", spawn "setxkbmap -layout 'se' -variant 'dvorak' -option 'ctrl:swapcaps'")
        , ("M-S-d", spawn "setxkbmap -layout 'us' -variant 'dvorak' -option 'ctrl:swapcaps'")
        , ("M-S-r", spawn "xmonad --restart")
        ]


------------------------------------------------------------------------
---LAYOUTS
------------------------------------------------------------------------
myLayoutHook = avoidStruts $ myDefaultLayout

    where
        myDefaultLayout = tall

tall = renamed [Replace "tall"] $ spacing 20 $ gaps [(U,20), (D,20), (L,20), (R,20)] $ Tall 1 (3/100) (1/2)


------------------------------------------------------------------------
---AUTOSTART
------------------------------------------------------------------------
myStartupHook = do
          spawnOnce "alacritty &"
          spawnOnce "emacs --daemon &" 
          spawnOnce "feh --bg-fill /home/kacper/Pictures/Wallpapers/Backdrops/Dazzled-Horizon.png &"
          spawnOnce "picom &"
          spawnOnce "setxkbmap -layout 'us' -variant 'dvorak' -option 'ctrl:swapcaps'" 
          setWMName "LG3D"
