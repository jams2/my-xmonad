{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Map qualified as Map
import Elisp
import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Actions.Submap (submap)
import XMonad.Hooks.DynamicLog (xmobar)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Layout.Spacing (Border (..), spacingRaw)
import XMonad.Util.EZConfig (additionalKeys)

myTerminal = "wezterm"

winKey = mod4Mask

class ToArgString a where
  toArgString :: a -> String

data EmacsClientArgs = EmacsClientArgs
  { emacsClientEval :: ElispExpr,
    emacsClientFrameParams :: ElispExpr,
    emacsClientFile :: String
  }

emacsClientDefaults :: EmacsClientArgs
emacsClientDefaults =
  EmacsClientArgs
    { emacsClientEval = [elisp|()|],
      emacsClientFrameParams = [elisp|()|],
      emacsClientFile = ""
    }

instance ToArgString EmacsClientArgs where
  toArgString args =
    let eval = case emacsClientEval args of
          EList [] -> ""
          expr -> "--eval " ++ show (prettyPrint expr)
        frameParams = case emacsClientFrameParams args of
          EList [] -> ""
          expr -> "--frame-parameters " ++ show (prettyPrint expr)
        file = emacsClientFile args
     in unwords [eval, frameParams, file]

getEmacsClientCommand :: EmacsClientArgs -> String
getEmacsClientCommand args = unwords ["emacsclient", "--create-frame", toArgString args]

orgAgendaEval =
  [elisp|
(let ((org-agenda-window-setup 'only-window))
  (org-agenda-list))
|]

orgAgendaArgs :: EmacsClientArgs
orgAgendaArgs =
  emacsClientDefaults
    { emacsClientEval = orgAgendaEval,
      emacsClientFrameParams = [elisp|((name . "*Org Agenda*"))|]
    }

orgCaptureEval = [elisp|(org-capture nil "g")|]

orgCaptureActionArgs :: EmacsClientArgs
orgCaptureActionArgs =
  emacsClientDefaults
    { emacsClientEval = orgCaptureEval,
      emacsClientFrameParams = [elisp|((name . "*My Org Capture*"))|]
    }

orgNextActionsCommand =
  [elisp|
(let ((org-agenda-window-setup 'only-window))
  (org-tags-view nil "TODO=\"NEXT\""))
|]

orgNextActionsArgs :: EmacsClientArgs
orgNextActionsArgs =
  emacsClientDefaults
    { emacsClientEval = orgNextActionsCommand,
      emacsClientFrameParams = [elisp|((name . "*My Org Capture*"))|]
    }

orgViewGtdFileArgs :: EmacsClientArgs
orgViewGtdFileArgs = emacsClientDefaults {emacsClientFile = "/home/joshua/org/gtd.org"}

emacsCalcArgs :: EmacsClientArgs
emacsCalcArgs =
  emacsClientDefaults
    { emacsClientEval = [elisp|(full-calc)|],
      emacsClientFrameParams = [elisp|((name . "*Calc*"))|]
    }

emacsViewXmonadConfArgs :: EmacsClientArgs
emacsViewXmonadConfArgs =
  emacsClientDefaults {emacsClientFile = "/home/joshua/projects/my-xmonad/app/xmonad.hs"}

myKeys =
  [ ( (winKey, xK_e),
      submap . Map.fromList $
        [ ( (0, xK_c),
            spawn $ getEmacsClientCommand emacsClientDefaults
          ),
          ( (0, xK_x),
            spawn $ getEmacsClientCommand emacsViewXmonadConfArgs
          ),
          ( (0, xK_equal),
            spawn $ getEmacsClientCommand emacsCalcArgs
          ),
          ( (0, xK_a),
            spawn $ getEmacsClientCommand orgAgendaArgs
          ),
          ( (0, xK_g),
            spawn $ getEmacsClientCommand orgViewGtdFileArgs
          ),
          ( (0, xK_n),
            spawn $ getEmacsClientCommand orgNextActionsArgs
          ),
          ( (shiftMask, xK_g),
            spawn $ getEmacsClientCommand orgCaptureActionArgs
          )
        ]
    ),
    -- Media keys
    ((0, xF86XK_AudioRaiseVolume), spawn "amixer -D pulse sset Master 10%+"),
    ((0, xF86XK_AudioLowerVolume), spawn "amixer -D pulse sset Master 10%-"),
    ((0, xF86XK_AudioMute), spawn "amixer -D pulse sset Master toggle"),
    ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10"),
    ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10")
  ]

myNormalBorderColor = "#555"

myFocusedBorderColor = "#cb1aaa"

myBorderWidth = 2

myManageHook =
  composeAll
    [ className =? "Mullvad VPN" --> doFloat <+> doShift "9",
      className =? "Pavucontrol" --> doFloat,
      className =? "vlc" --> doFloat,
      className =? "Breach" --> doFloat,
      className =? "Paradox Launcher" --> doFloat,
      title =? "*Org Agenda*" --> doCenterFloat,
      title =? "*My Org Capture*" --> doCenterFloat,
      title =? "*Calc*" --> doCenterFloat
    ]

myLayout =
  avoidStruts $
    spacingRaw False (Border 10 0 10 0) True (Border 0 10 0 10) True $
      Tall nmaster delta ratio ||| Full
  where
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100

myConfig =
  def
    { terminal = myTerminal,
      modMask = winKey,
      borderWidth = myBorderWidth,
      normalBorderColor = myNormalBorderColor,
      focusedBorderColor = myFocusedBorderColor,
      manageHook = myManageHook <+> def,
      layoutHook = myLayout
    }
    `additionalKeys` myKeys

main :: IO ()
main = xmonad =<< xmobar (ewmhFullscreen $ ewmh myConfig)
