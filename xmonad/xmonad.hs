import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Actions.CycleWindows
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutCombinators hiding ( (|||) )
import XMonad.Layout.NoBorders
import XMonad.Layout.Circle
import XMonad.Hooks.EwmhDesktops


main = do
    dzen <- spawnPipe myDzenStatus
    conky <- spawnPipe myDzenConky
    trayer <- spawnPipe myTrayer
    startup <- spawnPipe "/home/amol/.xmonad/startup.sh"
    xmonad $ ewmh defaultConfig
        { terminal = "gnome-terminal"
        , manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig
        , layoutHook =  avoidStruts $ (myLayoutHook ||| layoutHook defaultConfig)
        , logHook = myLogHook dzen
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , workspaces = myWorkspaces
        , normalBorderColor  = "#000000"
        , focusedBorderColor = "#0000ff"
        } `additionalKeysP` myKeys


myTrayer = "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 11 --transparent true --alpha 0 --tint 0x1A1A1A --height 16"

myDzenStatus = "dzen2 -w '655' -ta 'l'" ++ myDzenStyle
myDzenConky  = "conky -c ~/.xmonad/conkyrc | dzen2 -x '655' -w '560' -ta 'r'" ++ myDzenStyle
myDzenStyle  = " -e 'button2=;' -h '16' -fg '#BBBBBB' -bg '#1A1A1A' -fn 'Clean:size=8'"

myWorkspaces_img = [
    "^i(/home/amol/.xmonad/dzen2_icons/workspaces/web.xbm)",
    "^i(/home/amol/.xmonad/dzen2_icons/workspaces/term.xbm)",
    "^i(/home/amol/.xmonad/dzen2_icons/workspaces/dev.xbm)",
    "^i(/home/amol/.xmonad/dzen2_icons/workspaces/main.xbm)",
    "^i(/home/amol/.xmonad/dzen2_icons/workspaces/media.xbm)",
    "^i(/home/amol/.xmonad/dzen2_icons/workspaces/browse.xbm)",
    "^i(/home/amol/.xmonad/dzen2_icons/workspaces/apps.xbm)",
    "^i(/home/amol/.xmonad/dzen2_icons/workspaces/misc.xbm)",
    "^i(/home/amol/.xmonad/dzen2_icons/workspaces/extra.xbm)"]

myWorkspaces = ["^ca(1, xdotool key super+" ++ (show i) ++ ")" ++ s ++ "^ca()" | (s, i) <- zip myWorkspaces_img [1..9]]

myManageHook = composeAll . concat $
   [ [ className =? "Firefox" --> doShift (myWorkspaces !! 0) ]
    , [ className =? "Sublime_text" --> doShift (myWorkspaces !! 2) ]
    , [ className =? "Gvim" --> doShift (myWorkspaces !! 2) ]
    , [ className =? "Vlc" --> doShift (myWorkspaces !! 4) ]
    , [ className =? "Smplayer" --> doShift (myWorkspaces !! 4)]
    , [ className =? "Nautilus" --> doShift (myWorkspaces !! 5) ]
    , [ className =? "Gimp" --> doFloat ]]

customTile = (Tall 1 (2/100) (2/3))

myLayoutHook = smartBorders $ (customTile ||| Mirror customTile ||| Full ||| Circle)

myDzenPP  = dzenPP
    { ppCurrent = dzenColor "#000000" "#ffff00" . wrap " " " "
    , ppHidden  = dzenColor "#1a1a1a" "#888888" . wrap " " " "
    , ppHiddenNoWindows = dzenColor "#888888" "" . wrap " " " "
    , ppUrgent  = dzenColor "#000000" "#00ff00" . wrap " " " "
    , ppSep     = ": "
    , ppTitle   = dzenColor "#ffffff" ""
    , ppOrder   = \(ws:_:t:_) -> [ws, t]
    }

myLogHook h = dynamicLogWithPP $ myDzenPP { ppOutput = hPutStrLn h }

myKeys = [("M1-<Tab>"   , cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab ) -- classic alt-tab behaviour
         , ("M-<Return>" , spawn "dmenu_run -b"                         ) -- app launcher
         , ("M-e"        , spawn "nautilus"                      ) -- launch file manager
         , ("C-M1-l"     , spawn "/home/amol/.xmonad/lock.sh"              ) -- lock screen
         ]
