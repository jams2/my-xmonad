{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.Map qualified as Map
import Graphics.X11.ExtraTypes.XF86
import XMonad
import XMonad.Actions.CycleWS (toggleWS)
import XMonad.Actions.Submap (submap)
import XMonad.Hooks.DynamicLog (xmobar)
import XMonad.Hooks.EwmhDesktops (ewmh, ewmhFullscreen)
import XMonad.Hooks.ManageDocks (avoidStruts)
import XMonad.Hooks.ManageHelpers (doCenterFloat)
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing (Border (..), spacingRaw)
import XMonad.Util.EZConfig (additionalKeys)

myTerminal = "xterm"

winKey = mod4Mask

myKeys =
  [ 
    -- Media keys
    ((0, xF86XK_AudioRaiseVolume), spawn "amixer -D pulse sset Master 10%+"),
    ((0, xF86XK_AudioLowerVolume), spawn "amixer -D pulse sset Master 10%-"),
    ((0, xF86XK_AudioMute), spawn "amixer -D pulse sset Master toggle"),
    ((0, xF86XK_MonBrightnessUp), spawn "xbacklight -inc 10"),
    ((0, xF86XK_MonBrightnessDown), spawn "xbacklight -dec 10"),
    ((winKey, xK_Tab), toggleWS)
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
      smartBorders (Tall nmaster delta ratio ||| Full)
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
