{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImportQualifiedPost #-}

import Elisp
import Data.Map qualified as Map
import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Actions.Submap (submap)
import XMonad.Hooks.DynamicLog (xmobar)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Layout.Spacing (Border (..), spacingRaw)
import XMonad.Util.EZConfig (additionalKeys)

myTerminal :: String
myTerminal = "wezterm"

winKey :: KeyMask
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
    let eval = show $ prettyPrint (emacsClientEval args)
        frameParams = show $ prettyPrint (emacsClientFrameParams args)
        file = emacsClientFile args
     in unwords ["--eval", eval, "--frame-parameters", frameParams, file]

getEmacsClientCommand :: EmacsClientArgs -> String
getEmacsClientCommand args = unwords ["emacsclient", "--create-frame", toArgString args]

orgAgendaArgs :: EmacsClientArgs
orgAgendaArgs =
  emacsClientDefaults
    { emacsClientEval = [elisp|(let ((org-agenda-window-setup 'only-window)) (org-agenda-list))|],
      emacsClientFrameParams = [elisp|((name . "*Org Agenda*"))|]
    }

orgCaptureTodoArgs :: EmacsClientArgs
orgCaptureTodoArgs =
  emacsClientDefaults
    { emacsClientEval = [elisp|(let ((org-capture-mode-hook '(delete-other-windows))) (org-capture nil "g"))|],
      emacsClientFrameParams = [elisp|((name . "*Org Capture*"))|]
    }

orgNextActionsArgs :: EmacsClientArgs
orgNextActionsArgs =
  emacsClientDefaults
    { emacsClientEval = [elisp|(let ((org-agenda-window-setup 'only-window)) (org-tags-view nil "TODO=\"NEXT\""))|],
      emacsClientFrameParams = [elisp|((name . "*Org Capture*"))|]
    }

myKeys =
  [ ( (winKey, xK_e),
      submap . Map.fromList $
        [ ( (0, xK_c),
            spawn "emacsclient --create-frame"
          ),
          ( (0, xK_x),
            spawn "emacsclient --create-frame /home/joshua/dotfiles/xmonad/.config/xmonad/xmonad.hs"
          ),
          ( (0, xK_equal),
            spawn "emacsclient --create-frame --eval '(full-calc)'"
          ),
          ( (0, xK_a),
            spawn $ getEmacsClientCommand orgAgendaArgs
          ),
          ( (0, xK_g),
            spawn "emacsclient --create-frame /home/joshua/org/gtd.org"
          ),
          ( (0, xK_n),
            spawn $ getEmacsClientCommand orgNextActionsArgs
          ),
          ( (shiftMask, xK_g),
            spawn $ getEmacsClientCommand orgCaptureTodoArgs
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
      title =? "*Org Capture*" --> doCenterFloat
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
