import XMonad
import XMonad.Config.Kde (kde4Config)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks (avoidStruts)
import qualified XMonad.Hooks.ManageHelpers as ManageHelpers
import XMonad.Hooks.ManageHelpers ((-?>))
import XMonad.Hooks.SetWMName (setWMName)
import XMonad.Layout.LayoutHints (layoutHintsToCenter)
import XMonad.Layout.NoBorders (hasBorder, noBorders, smartBorders)
import XMonad.Layout.NoFrillsDecoration (noFrillsDeco, shrinkText)
import XMonad.Layout.ShowWName (showWName)
import XMonad.Layout.Spacing (smartSpacing)
import XMonad.Prompt
import XMonad.Prompt.FuzzyMatch
import XMonad.Prompt.Pass
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run

workspaceNames = ["WEB", "EMACS", "TERM", "MISC", "VID", "FILE", "7", "8", "9"]

dedicatedWorkspacesForApps :: ManageHook
dedicatedWorkspacesForApps = ManageHelpers.composeOne [
  className =? "Firefox"       -?> doShift $ head workspaceNames,
  className =? "Google-chrome" -?> doShift $ head workspaceNames,
  className =? "smplayer"      -?> doShift $ workspaceNames !! 4,
  className =? "mpv"           -?> doShift $ workspaceNames !! 4,
  className =? "Nautilus"      -?> doShift $ workspaceNames !! 5,
  className =? "Nemo"          -?> doShift $ workspaceNames !! 5
  ]

checkModal :: Query Bool
checkModal = ManageHelpers.isInProperty "_NET_WM_STATE" "_NET_WM_STATE_MODAL"

checkSkipTaskbar :: Query Bool
checkSkipTaskbar = ManageHelpers.isInProperty "_NET_WM_STATE" "_NET_WM_STATE_SKIP_TASKBAR"

myManageHook :: ManageHook
myManageHook =
  composeAll
    [ ManageHelpers.transience',
      ManageHelpers.isKDETrayWindow --> doIgnore,
      ManageHelpers.isFullscreen --> ManageHelpers.doFullFloat,
      ManageHelpers.isDialog --> doFloat,
      checkModal --> ManageHelpers.doCenterFloat,
      (className =? "plasmashell") --> doIgnore,
      (className =? "krunner") --> doIgnore <+> doFloat >> hasBorder False,
      dedicatedWorkspacesForApps
    ]

fg = "#f9f7dd"

bg = "#2f2f2f"

lightGray = "#888888"

midGray = "#676767"

darkGray = "#474747"

red = "#ff5a5f"

green = "#86cb92"

yellow = "#f1f0cc"

blue = "#07a0c3"

purple = "#a761c2"

cyan = "#6e98a4"

black = "#000000"

kdeBg = "#eff0f1"
kdeHighlight = "#3daee9"

monospaceFont = "xft:Iosevka:size=12"

myLayouts =
  twoThirdRatioTiling
    ||| Mirror twoThirdRatioTiling
    ||| Full
  where
    twoThirdRatioTiling = Tall 1 (2 / 100) (2 / 3)

myLayoutHook =
  showWName $
    avoidStruts $
      smartBorders $
        layoutHintsToCenter $
          smartSpacing 8 myLayouts

myStartupHook =
  do
    setWMName "LG3D"
    spawn "~/.xmonad/startup.sh"

myXPConfig :: XPConfig
myXPConfig = def {
  font  = monospaceFont,
  position = CenteredAt {
    xpCenterY = 0.4,
    xpWidth = 0.3
  },
  bgColor  = kdeBg,
  fgColor  = black,
  bgHLight  = kdeHighlight,
  fgHLight  = black,
  borderColor = red,
  promptBorderWidth = 4,
  height = 32,
  searchPredicate = fuzzyMatch,
  sorter = fuzzySort,
  complCaseSensitivity = CaseInSensitive,
  maxComplRows = Just 8,
  maxComplColumns = Just 1
}

myConfig =
  kde4Config
    { terminal = "urxvt",
      borderWidth = 4,
      modMask = mod4Mask,
      workspaces = workspaceNames,
      focusFollowsMouse = False,
      startupHook = myStartupHook <+> startupHook kde4Config,
      manageHook = myManageHook <+> manageHook kde4Config,
      layoutHook = myLayoutHook
    }

main =
  xmonad $ ewmhFullscreen $ ewmh $
    myConfig
      `additionalKeysP` [ ("M-<Return>", spawn "krunner"),
                          ("M-f", spawn "nemo --no-desktop"),
                          ("M-p", passPrompt myXPConfig),
                          ("M-d", spawn "dmenu_run"),
                          ("M-C-<Delete>", runInTerm "" "loginctl lock-session")
                        ]
