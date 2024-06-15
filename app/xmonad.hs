{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-orphans #-}

import Data.Map qualified as Map
import Elisp
import Graphics.X11.ExtraTypes.XF86
import SideBorder
import XMonad
import XMonad.Actions.CopyWindow (copy)
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.Submap (submap)
import XMonad.Config.Desktop (desktopConfig, desktopLayoutModifiers)
import XMonad.Config.Gnome (gnomeConfig)
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers (Side (CE), doCenterFloat, doSideFloat)
import XMonad.Hooks.ServerMode
import XMonad.Layout.CenteredIfSingle
import XMonad.Layout.Decoration (DecorationMsg (..))
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.StackSet qualified as W
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.ExtensibleState qualified as XS

home = "/home/jmunn/"

projectDir = home ++ "projects/"

orgDir = home ++ "org/"

myTerminal = "wezterm"

winKey = mod4Mask

class ToArgString a where
  toArgString :: a -> String

data EmacsClientArgs = EmacsClientArgs
  { emacsClientEval :: !ElispExpr,
    emacsClientFrameParams :: !ElispExpr,
    emacsClientFile :: !String
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
      emacsClientFrameParams = [elisp|((name . "*My Org Agenda*") (width . 120))|]
    }

orgCaptureActionEval = [elisp|(org-capture nil "g")|]

orgCaptureActionArgs :: EmacsClientArgs
orgCaptureActionArgs =
  emacsClientDefaults
    { emacsClientEval = orgCaptureActionEval,
      emacsClientFrameParams = [elisp|((name . "*My Org Capture*") (width . 120))|]
    }

orgCaptureNoteArgs :: EmacsClientArgs
orgCaptureNoteArgs =
  emacsClientDefaults
    { emacsClientEval = [elisp|(org-capture nil "n")|],
      emacsClientFrameParams = [elisp|((name . "*My Org Capture*") (width . 120))|]
    }

orgCaptureArgs :: EmacsClientArgs
orgCaptureArgs =
  emacsClientDefaults
    { emacsClientEval = [elisp|(josh/org-capture-focused)|],
      emacsClientFrameParams = [elisp|((name . "*My Org Capture*") (width . 120))|]
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
      emacsClientFrameParams = [elisp|((name . "*My Org Capture*") (width . 120))|]
    }

orgViewGtdFileArgs :: EmacsClientArgs
orgViewGtdFileArgs = emacsClientDefaults {emacsClientFile = orgDir ++ "gtd.org"}

orgViewNotesArgs :: EmacsClientArgs
orgViewNotesArgs = emacsClientDefaults {emacsClientFile = orgDir ++ "notes.org"}

emacsCalcArgs :: EmacsClientArgs
emacsCalcArgs =
  emacsClientDefaults
    { emacsClientEval = [elisp|(full-calc)|],
      emacsClientFrameParams = [elisp|((name . "*Calc*"))|]
    }

emacsViewXmonadConfArgs :: EmacsClientArgs
emacsViewXmonadConfArgs =
  emacsClientDefaults {emacsClientFile = projectDir ++ "my-xmonad/app/xmonad.hs"}

pythonScratchEval :: ElispExpr
pythonScratchEval =
  [elisp|
(let ((file (make-temp-file "py_scratch" nil ".py" "#!/usr/bin/python")))
  (find-file file))
|]

emacsPythonScratchBufferArgs :: EmacsClientArgs
emacsPythonScratchBufferArgs =
  emacsClientDefaults
    { emacsClientEval = pythonScratchEval,
      emacsClientFrameParams = [elisp|((name . "*Python Scratch*"))|]
    }

markDownScratchEval :: ElispExpr
markDownScratchEval =
  [elisp|
(let ((file (make-temp-file "md_scratch" nil ".md")))
  (find-file file))
|]

emacsMarkdownScratchBufferArgs :: EmacsClientArgs
emacsMarkdownScratchBufferArgs =
  emacsClientDefaults
    { emacsClientEval = markDownScratchEval,
      emacsClientFrameParams = [elisp|((name . "*MarkDown Scratch*"))|]
    }

newFrameEval :: ElispExpr
newFrameEval =
  [elisp|(fancy-startup-screen)|]

emacsNewFrameArgs :: EmacsClientArgs
emacsNewFrameArgs = emacsClientDefaults {emacsClientEval = newFrameEval}

myKeys =
  [ ( (winKey, xK_e),
      submap . Map.fromList $
        [ ( (0, xK_c),
            spawn $ getEmacsClientCommand emacsNewFrameArgs
          ),
          ( (shiftMask, xK_c),
            spawn $ getEmacsClientCommand orgCaptureArgs
          ),
          ( (0, xK_r),
            spawn "systemctl restart --user emacs.service"
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
          ( (shiftMask, xK_g),
            spawn $ getEmacsClientCommand orgCaptureActionArgs
          ),
          ( (0, xK_l),
            spawn $ getEmacsClientCommand orgNextActionsArgs
          ),
          ( (0, xK_n),
            spawn $ getEmacsClientCommand orgViewNotesArgs
          ),
          ( (shiftMask, xK_n),
            spawn $ getEmacsClientCommand orgCaptureNoteArgs
          ),
          ( (0, xK_p),
            spawn $ getEmacsClientCommand emacsPythonScratchBufferArgs
          ),
          ( (0, xK_m),
            spawn $ getEmacsClientCommand emacsMarkdownScratchBufferArgs
          )
        ]
    ),
    ((winKey .|. shiftMask, xK_s), spawn "gnome-control-center"),
    -- Media keys
    ((winKey, xK_F8), spawn "amixer -D pulse sset Master toggle"),
    ((winKey, xK_F9), spawn "amixer -D pulse sset Master 10%-"),
    ((winKey, xK_F10), spawn "amixer -D pulse sset Master 10%+"),
    ((0, xF86XK_AudioRaiseVolume), spawn "amixer -D pulse sset Master 10%+"),
    ((0, xF86XK_AudioLowerVolume), spawn "amixer -D pulse sset Master 10%-"),
    ((0, xF86XK_AudioMute), spawn "amixer -D pulse sset Master toggle"),
    ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10"),
    ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10"),
    ((winKey, xK_Tab), toggleWS),
    ((winKey, xK_v), spawn "pavucontrol"),
    ((winKey, xK_Print), spawn "flameshot gui")
  ]
    -- mod 1-9 switch to workspace
    -- mod-shift 1-9 move client to workspace
    -- mod-control-shift 1-9 copy client to workspace
    ++ [ ((winKey .|. m, k), windows $ f i)
         | (i, k) <- zip myWorkspaces [xK_1 ..],
           (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]
       ]

myWorkspaces = map show ([1 .. 9] :: [Int])

myManageHook =
  composeAll
    [ className =? "Pavucontrol" --> doCenterFloat,
      className =? "Gnome-control-center" --> doCenterFloat,
      className =? "Gnome-panel" <&&> title =? "Run Application" --> doCenterFloat,
      className =? "Evolution-alarm-notify" --> doSideFloat CE,
      className =? "zoom" --> doCenterFloat,
      title =? "*My Org Agenda*" --> doCenterFloat,
      title =? "*My Org Capture*" --> doCenterFloat,
      title =? "*Calc*" --> doCenterFloat,
      className =? "qmmp" --> hasBorder False <+> doFloat,
      className =? "Qmmp" --> hasBorder False <+> doFloat,
      appName =? "playlist" <&&> className =? "Qmmp" --> hasBorder False <+> doFloat
    ]

myLayout =
    draggingVisualizer $
      desktopLayoutModifiers $
        smartBorders $
          spacingRaw False (Border 10 0 10 0) True (Border 0 10 0 10) True $
            myTall
              ||| centeredIfSingle 0.7 0.98 myTall
              ||| Full
  where
    nmaster = 1
    ratio = 1 / 2
    delta = 3 / 100
    myTall = Tall nmaster delta ratio

borderSide = U

borderSize = 12

sideBorderConfig :: SideBorderConfig
sideBorderConfig =
  def
    { sbSide = borderSide,
      sbSize = borderSize,
      sbActiveColor = "#583435",
      sbActiveBorderColor = "#583435",
      sbInactiveColor = "#372d32",
      sbInactiveBorderColor = "#372d32"
    }

instance ExtensionClass SideBorderConfig where
  initialValue = sideBorderConfig

updateBorderColors :: String -> X ()
updateBorderColors newColors = do
  conf <- XS.gets updateConf :: X SideBorderConfig
  broadcastMessage (SetTheme $ getTheme conf) >> refresh
  XS.put conf
  where
    updateConf c =
      c
        { sbActiveColor = ac,
          sbActiveBorderColor = ac,
          sbInactiveColor = ic,
          sbInactiveBorderColor = ic
        }
    -- TODO: parse the input properly - "#colour,#colour"
    ac = takeWhile (/= ',') newColors
    ic = tail $ dropWhile (/= ',') newColors

updateBorderColorsAtom = "XMONAD_UPDATE_BORDER_COLORS"

myHandleEventHook = serverModeEventHookF updateBorderColorsAtom updateBorderColors

myNormalBorderColor = "#555"

myFocusedBorderColor = "#cb1aaa"

myBorderWidth = 3

main :: IO ()
main = do
  xmonad $
    ewmhFullscreen $
      ewmh
        gnomeConfig
          { terminal = myTerminal,
            modMask = winKey,
            manageHook = myManageHook <> manageHook gnomeConfig,
            layoutHook = myLayout,
            handleEventHook = myHandleEventHook <> handleEventHook desktopConfig,
            normalBorderColor = myNormalBorderColor,
            focusedBorderColor = myFocusedBorderColor,
            borderWidth = myBorderWidth
          }
        `additionalKeys` myKeys
