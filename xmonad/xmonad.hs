import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWindows
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeysP)
import System.IO
import XMonad.Layout.Spacing
import XMonad.Layout.LayoutCombinators hiding ( (|||) )
import XMonad.Layout.NoBorders
import XMonad.Layout.Circle
import XMonad.Layout.Grid
import qualified XMonad.Layout.Magnifier as Magnifier
import qualified Data.Map as Map

main = do
    dzenTop <- spawnPipe myDzenTopBar
    dzenBottom <- spawnPipe myDzenBottomBar
    conkyTop <- spawnPipe myConkyTop
    conkyBottom <- spawnPipe myConkyBottom

    trayer <- spawnPipe myTrayer
    startup <- spawnPipe "/home/amol/.xmonad/startup.sh"

    xmonad $ ewmh defaultConfig
        { terminal = "gnome-terminal"
        , manageHook = myManageHook <+> manageDocks <+> manageHook defaultConfig
        , layoutHook =  avoidStruts $ (myLayoutHook ||| layoutHook defaultConfig)
        , logHook = myLogHook dzenTop dzenBottom
        , modMask = mod4Mask     -- Rebind Mod to the Windows key
        , workspaces = myWorkspaces
        , focusFollowsMouse = False
        , normalBorderColor  = "#888888"
        , focusedBorderColor = "#0000ff"
        , borderWidth = 3
        } `additionalKeysP` myKeys


-- Colors
darkBlack  = "#020202"
lightBlue  = "#44AACC"
fgGray     = "#9D9D9D"
midGray    = "#444444"
darkGray   = "#101010"
lightGreen = "#66FF66"

dzenFont sz = "Clean:size=" ++ (show sz)

myTrayer = "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 11 --transparent true --alpha 0 --tint 0x020202 --height 16"


myDzenStyle  = " -e 'button2=;' -h '16' -fg '" ++ fgGray ++ "' -bg '" ++ darkBlack ++ "' -fn '" ++ dzenFont 10 ++ "'"
myDzenTopBar = "dzen2 -w '850' -ta 'l'" ++ myDzenStyle
myDzenBottomBar = "dzen2 -y '752' -w '400' -ta 'l'" ++ myDzenStyle

myConkyTop = "conky -c ~/.xmonad/conky_utilities/conky_top_rc | dzen2 -x '850' -w '366' -ta 'r'" ++ myDzenStyle
myConkyBottom = "conky -c ~/.xmonad/conky_utilities/conky_bottom_rc | dzen2 -y '752' -x '400' -w '966' -ta 'r'" ++ myDzenStyle
-- myDzenConky  = "conky -c ~/.xmonad/conkyrc | dzen2 -x '656' -w '560' -ta 'r'" ++ myDzenStyle

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
-- myWorkspaces = ["^ca(1, xdotool key super+" ++ (show i) ++ ")" ++ (show i) ++ "^ca()" | i <- [1..9]]

myManageHook = composeAll . concat $
   [ [ className =? "Firefox"             --> doShift (myWorkspaces !! 0) ]
    , [ className =? "Sublime_text"       --> doShift (myWorkspaces !! 2) ]
    , [ className =? "Gvim"               --> doShift (myWorkspaces !! 2) ]
    , [ className =? "Vlc"                --> doShift (myWorkspaces !! 4) ]
    , [ className =? "Smplayer"           --> doShift (myWorkspaces !! 4) ]
    , [ className =? "Nautilus"           --> doShift (myWorkspaces !! 5) ]
    , [ className =? "Pcmanfm"            --> doShift (myWorkspaces !! 5) ]
    , [ className =? "Nemo"               --> doShift (myWorkspaces !! 5) ]
    , [ className =? "Thunderbird"        --> doShift (myWorkspaces !! 8) ]
    , [ className =? "Gimp"               --> doFloat ]]

customTile = (Tall 1 (2/100) (2/3))
magnify = Magnifier.magnifiercz 1.0

myLayoutHook = smartBorders $ smartSpacing 5 $ (customTile ||| Mirror customTile ||| Full ||| magnify Circle ||| magnify Grid)

myTopDzenPP  = dzenPP
    {
    ppCurrent = dzenColor darkBlack lightBlue . wrap " " " ",
    ppHidden  = dzenColor darkGray fgGray. wrap " " " ",
    ppHiddenNoWindows = dzenColor darkBlack darkGray . wrap " " " ",
    ppUrgent  = dzenColor darkBlack lightGreen . wrap " " " ",

    -- ppWsSep = "^fn(" ++ dzenFont 2 ++ ") ^fn(" ++ dzenFont 10 ++ ")",
    ppSep     = "^fn(" ++ dzenFont 5 ++ ") ^fn(" ++ dzenFont 10 ++ ")",

    ppTitle   = dzenColor darkGray fgGray . wrap "  " "  ",

    ppOrder   = \(ws:l:t:w) -> [ws, t]
    }
myBottomDzenPP = dzenPP
    {
    ppLayout  = dzenColor fgGray darkGray . wrap " " " ",
    ppSep     = "^fn(" ++ dzenFont 5 ++ ") ^fn(" ++ dzenFont 10 ++ ")",
    ppOrder = \(ws:l:t:w) -> l:w
    }

myLogHook top bottom = do
    dynamicLogWithPP $ myTopDzenPP { ppOutput = hPutStrLn top }
    dynamicLogWithPP $ myBottomDzenPP { ppOutput = hPutStrLn bottom }

myKeys = [("M1-<Tab>"   , cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab ) -- classic alt-tab behaviour
         , ("M-<Return>" , spawn "dmenu_run -b" ) -- app launcher
         , ("M-e"        , spawn "nemo --no-desktop" ) -- launch file manager
         , ("C-M1-l"     , spawn "/home/amol/.xmonad/lock.sh" ) -- lock screen
         , ("M-S--", sendMessage Magnifier.MagnifyLess )
         , ("M-S-=", sendMessage Magnifier.MagnifyMore )
         ]