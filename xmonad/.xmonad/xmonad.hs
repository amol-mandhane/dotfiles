import qualified Data.Map                     as Map
import qualified System.IO                    as SysIO
import           Text.Printf                  (printf)
import qualified XMonad                       as X
import qualified XMonad.Actions.CycleWindows  as CycleWindows
import qualified XMonad.Hooks.DynamicLog      as DLog
import qualified XMonad.Hooks.EwmhDesktops    as EwmhDesktops
import qualified XMonad.Hooks.ManageDocks     as ManageDocks
import qualified XMonad.Hooks.SetWMName       as SetWMName
import qualified XMonad.Layout.Circle         as LayoutCircle
import qualified XMonad.Layout.Grid           as LayoutGrid
import qualified XMonad.Layout.LayoutModifier as LayoutModifier
import qualified XMonad.Layout.Magnifier      as LayoutMagnifier
import qualified XMonad.Layout.NoBorders      as LayoutNoBorders
import qualified XMonad.Layout.Spacing        as LayoutSpacing
import           XMonad.Util.EZConfig         (additionalKeysP)
import           XMonad.Util.Run              (spawnPipe)
import qualified XMonadConfig                 as XMonadConfig

-- Colors
darkBlack :: String
lightBlue :: String
midBlue :: String
fgGray :: String
midGray :: String
darkGray :: String
lightGreen :: String

darkBlack  = "#020202"
lightBlue  = "#44AACC"
midBlue    = "#008888"
fgGray     = "#9D9D9D"
midGray    = "#888888"
darkGray   = "#101010"
lightGreen = "#66FF66"


-- Display bars config
-- Trayer
trayerCommand :: String
trayerCommand = "trayer " ++
  "--edge top --align right " ++
  "--SetDockType true --SetPartialStrut true --expand true " ++
  "--transparent true --alpha 0 --tint 0x020202 " ++
  "--width 11 --height 16"

dzenFont :: Int -> String
dzenFont sz = "M+1m:Light:size=" ++ (show sz)

-- Dzen
dzenBarStyle :: String
dzenBarStyle = " -e 'button2=;' " ++
  (printf "-h '%d' " XMonadConfig.dzenBarHeight) ++
  (printf "-fg '%s' " fgGray) ++
  (printf "-bg '%s' " darkBlack) ++
  (printf "-fn '%s' " $ dzenFont 9)

dzenTopLeftBarCommand :: String
dzenTopLeftBarCommand = printf format width dzenBarStyle
  where
    format = "dzen2 -w '%d' -ta 'l' %s"
    width = XMonadConfig.dzenTopLeftBarWidth

dzenBottomLeftBarCommand :: String
dzenBottomLeftBarCommand = printf format topOffset width dzenBarStyle
  where
    format = "dzen2 -y '%d' -w '%d' -ta 'l' %s"
    width = XMonadConfig.dzenBottomLeftBarWidth
    topOffset = XMonadConfig.screenHeight - XMonadConfig.dzenBarHeight

-- Additional dzen bars with conky information
conkyTopRightCommand :: String
conkyTopRightCommand = "conky -c ~/.xmonad/conky/conky_top_rc | " ++ dzenPipe
  where
    horizontalOffset = XMonadConfig.dzenTopLeftBarWidth
    width =
      XMonadConfig.screenWidth
        - horizontalOffset
        - XMonadConfig.trayerEstimatedWidth
    dzenPipe =
      printf
        "dzen2 -x '%d' -w '%d' -ta 'r' %s"
         horizontalOffset
         width
         dzenBarStyle

conkyBottomRightCommand :: String
conkyBottomRightCommand =
  "conky -c ~/.xmonad/conky/conky_bottom_rc | " ++ dzenPipe
  where
    horizontalOffset = XMonadConfig.dzenBottomLeftBarWidth
    verticalOffset = XMonadConfig.screenHeight - XMonadConfig.dzenBarHeight
    width = XMonadConfig.screenWidth - horizontalOffset
    dzenPipe =
      printf
        "dzen2 -x '%d' -y '%d' -w '%d' -ta 'r' %s"
        horizontalOffset
        verticalOffset
        width
        dzenBarStyle

workspaceList :: [String]
workspaceList = ["WEB", "EMACS", "TERM", "MISC", "VID", "FILE", "7", "8", "9"]

-- Move specific windows to their workspaces automatically
windowShiftSpec :: String -> Int -> X.ManageHook
windowShiftSpec className workspaceId =
  X.className X.=? className X.-->
    X.doShift (workspaceList !! workspaceId)

windowRelocatorManageHook :: X.ManageHook
windowRelocatorManageHook = X.composeAll [
  windowShiftSpec "Firefox"       0,
  windowShiftSpec "Google-chrome" 0,
  windowShiftSpec "smplayer"      4,
  windowShiftSpec "mpv"           4,
  windowShiftSpec "Nautilus"      5,
  windowShiftSpec "Nemo"          5,
  X.className X.=? "Gimp" X.--> X.doFloat
  ]

-- Layout management
twoThirdRatioTiling :: X.Tall a
twoThirdRatioTiling = X.Tall 1 (2/100) (2/3)

magnify :: l a -> LayoutModifier.ModifiedLayout LayoutMagnifier.Magnifier l a
magnify = LayoutMagnifier.magnifiercz 1.0

customLayouts =
  LayoutNoBorders.smartBorders $
    LayoutSpacing.smartSpacing 5 $
      (twoThirdRatioTiling X.|||
       X.Mirror twoThirdRatioTiling X.|||
       X.Full X.|||
       magnify LayoutCircle.Circle X.|||
       magnify LayoutGrid.Grid)

-- Dzen XMonad log bars format config
dzenTopLog :: DLog.PP
dzenTopLog  = DLog.dzenPP {
  DLog.ppCurrent = (DLog.dzenColor darkBlack lightBlue).(DLog.wrap " " " "),
  DLog.ppHidden  = (DLog.dzenColor darkGray fgGray). (DLog.wrap " " " "),

  DLog.ppHiddenNoWindows = (DLog.dzenColor darkBlack darkGray).(DLog.wrap " " " "),
  DLog.ppUrgent  = (DLog.dzenColor darkBlack lightGreen).(DLog.wrap " " " "),

  DLog.ppSep     = "^fn(" ++ dzenFont 5 ++ ") ^fn(" ++ dzenFont 10 ++ ")",

  DLog.ppTitle   = (DLog.dzenColor darkGray fgGray) . (DLog.wrap "  " "  "),

  DLog.ppOrder   = \(ws:_:t:_) -> [ws, t]
}

dzenBottomLog :: DLog.PP
dzenBottomLog = DLog.dzenPP {
    DLog.ppLayout  = (DLog.dzenColor fgGray darkGray) . (DLog.wrap " " " "),
    DLog.ppSep     = "^fn(" ++ dzenFont 5 ++ ") ^fn(" ++ dzenFont 10 ++ ")",
    DLog.ppOrder = \(_:l:_:w) -> l:w
}

dzenLogHook :: SysIO.Handle -> SysIO.Handle -> X.X ()
dzenLogHook top bottom = do
    DLog.dynamicLogWithPP $ dzenTopLog { DLog.ppOutput = SysIO.hPutStrLn top }
    DLog.dynamicLogWithPP $ dzenBottomLog { DLog.ppOutput = SysIO.hPutStrLn bottom }

-- Shortcuts
customKeymap :: [(String, X.X())]
customKeymap = [
  -- Alt-TAB
  ("M1-<Tab>", CycleWindows.cycleRecentWindows [X.xK_Alt_L] X.xK_Tab X.xK_Tab),
  -- Dmenu on S-RET
  ("M-<Return>", X.spawn "dmenu_run -b"),
  -- S-e file browser
  ("M-e", X.spawn "nemo --no-desktop" ),
  -- Ctrl-Alt-L lock screen
  ("C-M1-l"     , X.spawn "~/.xmonad/lock.sh"),

  -- S-Shift-+\- Magnifier
  ("M-S--", X.sendMessage LayoutMagnifier.MagnifyLess),
  ("M-S-=", X.sendMessage LayoutMagnifier.MagnifyMore),

  -- Keyboard Volume buttons
  ("<XF86AudioMute>", X.spawn "amixer -q sset Master toggle"),
  ("<XF86AudioRaiseVolume>", X.spawn "amixer -q sset Master 1%+"),
  ("<XF86AudioLowerVolume>", X.spawn "amixer -q sset Master 1%-")
  ]

main :: IO ()
main = do
    dzenTop <- spawnPipe dzenTopLeftBarCommand
    dzenBottom <- spawnPipe dzenBottomLeftBarCommand

    _ <- spawnPipe conkyTopRightCommand
    _ <- spawnPipe conkyBottomRightCommand

    _ <- spawnPipe trayerCommand
    _ <- spawnPipe "~/.xmonad/startup.sh"

    X.xmonad $ EwmhDesktops.ewmh X.defaultConfig
        { X.terminal = "gnome-terminal"
        , X.manageHook =
            windowRelocatorManageHook X.<+>
            ManageDocks.manageDocks X.<+>
            X.manageHook X.defaultConfig
        , X.layoutHook =
            ManageDocks.avoidStruts $
            (customLayouts X.||| X.layoutHook X.defaultConfig)
        , X.logHook = dzenLogHook dzenTop dzenBottom
        , X.modMask = X.mod4Mask     -- Rebind Mod to the Windows key
        , X.workspaces = workspaceList
        , X.focusFollowsMouse = False
        , X.normalBorderColor  = midGray
        , X.focusedBorderColor = midBlue
        , X.borderWidth = 1
        , X.startupHook = SetWMName.setWMName "LG3D"
        } `additionalKeysP` customKeymap
