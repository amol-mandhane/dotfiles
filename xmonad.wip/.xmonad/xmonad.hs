import           Codec.Binary.UTF8.String         (decodeString)
import           Data.Monoid                      (All)
import qualified DBus
import qualified DBus.Client                      as DBusClient
import           Graphics.X11.Xlib.Cursor         (xC_left_ptr)
import           System.Directory                 (doesDirectoryExist)
import qualified XMonad                           as X
import           XMonad.Actions.CopyWindow        (copyToAll, kill1,
                                                   killAllOtherCopies,
                                                   wsContainingCopies)
import           XMonad.Core                      (ManageHook, WorkspaceId,
                                                   spawn)
import           XMonad.Hooks.DynamicLog          (PP (..), dynamicLogWithPP)
import           XMonad.Hooks.EwmhDesktops        (ewmh, ewmhDesktopsLogHook)
import           XMonad.Hooks.ManageDocks         (avoidStruts, docks,
                                                   manageDocks)
import           XMonad.Hooks.ManageHelpers       (composeOne, (-?>))
import           XMonad.Hooks.SetWMName           (setWMName)
import           XMonad.Hooks.UrgencyHook         (UrgencyHook, urgencyHook,
                                                   withUrgencyHook)
import           XMonad.Layout.Fullscreen         (fullscreenEventHook,
                                                   fullscreenFull,
                                                   fullscreenManageHook)
import           XMonad.Layout.LayoutHints        (layoutHintsToCenter)
import           XMonad.Layout.NoBorders          (noBorders)
import           XMonad.Layout.NoFrillsDecoration (noFrillsDeco)
import           XMonad.Layout.ShowWName          (showWName)
import           XMonad.Layout.SimpleDecoration   (Theme (..), shrinkText)
import           XMonad.Layout.Simplest           (Simplest (..))
import           XMonad.Layout.Spacing            (smartSpacing)
import           XMonad.Layout.SubLayouts         (GroupMsg (UnMerge), onGroup,
                                                   pullGroup, subLayout)
import           XMonad.Layout.Tabbed             (addTabs)
import           XMonad.Layout.WindowNavigation   (windowNavigation)
import           XMonad.StackSet                  (focusDown', focusUp')
import qualified XMonad.StackSet                  as XStack
import           XMonad.Util.Cursor               (setDefaultCursor)
import           XMonad.Util.EZConfig             (additionalKeysP)
import           XMonad.Util.NamedWindows         (getName)
import           XMonad.Util.Run                  (hPutStrLn, safeSpawn)
import           XMonad.Util.Types                (Direction2D (D, L, R, U))

-- lightGray = "#93a1a1"
-- gray    = "#333333"
-- yellow  = "#b58900"
-- orange  = "#cb4b16"
-- red     = "#dc322f"
-- magenta = "#d33682"
-- violet  = "#6c71c4"
-- blue    = "#268bd2"
-- cyan    = "#2aa198"
-- green   = "#859900"

fg = "#f9f7dd"
bg = "#2f2f2f"
lightGray = "#888888"
midGray = "#676767"
darkGray = "#474747"
red = "#ff5a5f"
green  = "#86cb92"
yellow = "#f1f0cc"
blue = "#07a0c3"
purple = "#a761c2"
cyan = "#6e98a4"
black = "#000000"

roboto  = "xft:Roboto:size=12"
gap     = 10
topBar  = 10

main :: IO ()
main = do
  dBusClient <- createDBusClient
  X.xmonad
    $ ewmh
    $ docks
    $ withUrgencyHook LibNotifyUrgencyHook
    $ myConfig dBusClient
    `additionalKeysP` myKeys

myConfig dBusClient = X.def {
  X.terminal = "urxvt",
  X.borderWidth = 4,
  X.focusedBorderColor = green,
  X.normalBorderColor = midGray,
  X.modMask = X.mod4Mask,
  X.workspaces = workspaces,
  X.layoutHook = myLayoutHook,
  X.logHook = myLogHook dBusClient,
  X.manageHook = myManageHook,
  X.handleEventHook = myHandleEventHook,
  X.startupHook = myStartupHook,
  X.focusFollowsMouse = False
}

topBarTheme :: Theme
topBarTheme = X.def {
  fontName = roboto,
  activeBorderColor = blue,
  activeColor = blue,
  activeTextColor = black,
  inactiveColor = darkGray,
  inactiveTextColor = black,
  inactiveBorderColor = darkGray,
  urgentColor = red,
  urgentTextColor = black,
  urgentBorderColor = red,
  decoHeight = 24
}

tabTheme :: Theme
tabTheme = X.def {
  fontName = roboto,
  activeBorderColor = blue,
  activeColor = blue,
  activeTextColor = black,
  inactiveColor = darkGray,
  inactiveTextColor = lightGray,
  inactiveBorderColor = darkGray,
  urgentColor = red,
  urgentTextColor = black,
  urgentBorderColor = red
}

workspaces = ["WEB", "EMACS", "TERM", "MISC", "VID", "FILE", "7", "8", "9"]

myLayoutHook = showWName $
  avoidStruts $
  noBorders $
  topBar $
  -- Tabs
  windowNavigation $
  addTabs shrinkText tabTheme $
  subLayout [] Simplest $
  -- End Tabs
  layoutHintsToCenter $
  smartSpacing gap $
  myLayouts
  where
    topBar = noFrillsDeco shrinkText topBarTheme

myLogHook :: DBusClient.Client -> X.X ()
myLogHook dBusClient = polybarWorkspacesLogHook dBusClient

polybarWorkspacesLogHook :: DBusClient.Client -> X.X ()
polybarWorkspacesLogHook dBusClient = do
  ewmhDesktopsLogHook
  dynamicLogWithPP $ X.def {
    ppCurrent             = polybarWorkspaceFormat bg blue,
    ppVisible             = polybarWorkspaceFormat bg yellow,
    ppUrgent              = polybarWorkspaceFormat bg red,
    ppHidden              = polybarWorkspaceFormat fg bg,
    ppHiddenNoWindows     = polybarWorkspaceFormat darkGray bg,
    ppWsSep               = "",
    ppSep                 = "",
    ppOrder               = \(ws:_:t:_) -> [ws, t],
    ppTitle               = \_ -> "",
    ppOutput              = signalToDBus dBusClient
    }

myManageHook :: ManageHook
myManageHook = manageDocks X.<+>
  resourceUserConfigManagerHook X.<+>
  fullscreenManageHook X.<+>
  X.manageHook X.def

myHandleEventHook :: X.Event -> X.X All
myHandleEventHook = fullscreenEventHook X.<+> X.handleEventHook X.def

myStartupHook :: X.X()
myStartupHook = do
  setWMName "LG3D"
  setDefaultCursor xC_left_ptr
  spawn "~/.xmonad/startup.sh"

myKeys :: [(String, X.X ())]
myKeys = [
  -- Keys for tabbed layout navigation
  ("M-C-k", X.sendMessage $ pullGroup U),
  ("M-C-h", X.sendMessage $ pullGroup L),
  ("M-C-j", X.sendMessage $ pullGroup D),
  ("M-C-l", X.sendMessage $ pullGroup R),
  ("M-'" , onGroup focusUp'),
  ("M-;" , onGroup focusDown'),
  ("M-p" , X.withFocused (X.sendMessage . UnMerge)),

  -- Picture-in-picture
  ("M-S-p", togglePictureInPicture),
  ("M-d", kill1),

  -- Launchers
  ("M-<Return>", spawn "rofi -show run"),
  ("M-f", spawn "nemo --no-desktop"),
  ("M-C-<Delete>" , spawn "xautolock -locknow"),

  -- Keyboard Volume buttons
  ("<XF86AudioMute>", spawn "amixer -q sset Master toggle"),
  ("<XF86AudioRaiseVolume>", spawn "amixer -q sset Master 1%+"),
  ("<XF86AudioLowerVolume>", spawn "amixer -q sset Master 1%-"),
  -- Keyboard brightness buttons
  ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 5"),
  ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 5")
  ]

-- Send desktop notifications on urgency
data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)
instance UrgencyHook LibNotifyUrgencyHook where
  urgencyHook LibNotifyUrgencyHook w = do
    name     <- getName w
    Just idx <- fmap (XStack.findTag w) $ X.gets X.windowset
    safeSpawn "notify-send" [show name, "workspace " ++ idx]

polybarWorkspaceFormat :: String -> String -> WorkspaceId -> String
polybarWorkspaceFormat foreground background workspaceId =
  "%{F" ++ foreground ++ " B" ++ background ++ "}  " ++ icon workspaceId ++ "  %{F- B-}"
  where
    icon w | w == "WEB"   = "\62057"
           | w == "EMACS" = "\61982"
           | w == "TERM" = "\61728"
           | w == "VID" = "\61448"
           | w == "FILE" = "\61563"
           | otherwise = "\62060"

dBusInterface = "user.xmonad.log"
dBusPath = "/user/xmonad/log"
dBusMember = "DynamicLogWithPP"

createDBusClient :: IO DBusClient.Client
createDBusClient = do
  client <- DBusClient.connectSession
  DBusClient.requestName client (DBus.busName_ dBusInterface) dBusParams
  return client
  where
    dBusParams = [
      DBusClient.nameAllowReplacement,
      DBusClient.nameReplaceExisting,
      DBusClient.nameDoNotQueue]

signalToDBus :: DBusClient.Client -> String -> IO ()
signalToDBus client message =  do
  let signal = (DBus.signal objectPath interfaceName memberName) {
    DBus.signalBody = [DBus.toVariant $ decodeString message]
  }
  DBusClient.emit client signal
  where
    objectPath = DBus.objectPath_ dBusPath
    interfaceName = DBus.interfaceName_ dBusInterface
    memberName = DBus.memberName_ dBusMember

togglePictureInPicture :: X.X ()
togglePictureInPicture =
  wsContainingCopies >>=
    \ws -> case ws of
         [] -> X.windows copyToAll
         _  -> killAllOtherCopies

resourceUserConfigManagerHook :: ManageHook
resourceUserConfigManagerHook = composeOne [
  X.className X.=? "Firefox"       -?> X.doShift $ workspaces !! 0,
  X.className X.=? "Google-chrome" -?> X.doShift $ workspaces !! 0,
  X.className X.=? "smplayer"      -?> X.doShift $ workspaces !! 4,
  X.className X.=? "mpv"           -?> X.doShift $ workspaces !! 4,
  X.className X.=? "Nautilus"      -?> X.doShift $ workspaces !! 5,
  X.className X.=? "Nemo"          -?> X.doShift $ workspaces !! 5
  ]

myLayouts =
  twoThirdRatioTiling X.|||
  X.Mirror twoThirdRatioTiling X.|||
  X.Full
  where
    twoThirdRatioTiling = X.Tall 1 (2/100) (2/3)
