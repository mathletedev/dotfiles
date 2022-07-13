import System.IO

import qualified Data.Map as M

import XMonad hiding ((|||))
import XMonad.Actions.CopyWindow
import XMonad.Actions.NoBorders
import XMonad.Actions.SpawnOn
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.GridVariants
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig
import XMonad.Util.Run

import Graphics.X11.ExtraTypes.XF86

myTerminal = "kitty"

myBrowser = "firefox-developer-edition"

myModMask = mod4Mask

myBorderWidth = 0

myWorkspaces = [" <fn=1>\xf015</fn> ", " <fn=1>\xf1c9</fn> ", " <fn=1>\xf0ac</fn> ", " <fn=1>\xf4ad</fn> ", " <fn=1>\xf7d9</fn> ", " <fn=1>\xf233</fn> "]

mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

myManageHook = composeAll [
	title =? "kitty" --> doRectFloat (W.RationalRect 0.375 0.375 0.25 0.25)
	] <+> manageDocks <+> manageSpawn

tall =
	renamed [Replace "tall"] $
 	mySpacing 8 $
 	ResizableTall 1 (3 / 100) (1 / 2) []

wide =
 	renamed [Replace "wide"] $
 	mySpacing 8 $
 	Mirror (ResizableTall 1 (3 / 100) (1 / 2) [])

grid =
 	renamed [Replace "grid"] $
 	mySpacing 8 $
 	Grid (16 / 10)

full =
 	renamed [Replace "full"] $
 	mySpacing 8 $
 	Full

myLayoutHook = avoidStruts $ tall ||| wide ||| grid ||| full

myStartupHook = do
	spawnOn (myWorkspaces !! 0) "notion-app"
	spawnOn (myWorkspaces !! 1) "kitty"
	spawnOn (myWorkspaces !! 2) "firefox-developer-edition"
	spawnOn (myWorkspaces !! 4) "dolphin"
	spawnOn (myWorkspaces !! 5) "systemsettings"

myLayoutPrinter "tall" = "<fn=1>\xf338</fn>"
myLayoutPrinter "wide" = "<fn=1>\xf337</fn>"
myLayoutPrinter "grid" = "<fn=1>\xf047</fn>"
myLayoutPrinter "full" = "<fn=1>\xf31e</fn>"
myLayoutPrinter x = x

myLogHook xmproc0 xmproc1 = dynamicLogWithPP $ xmobarPP {
	ppOutput = \x -> hPutStrLn xmproc0 x >> hPutStrLn xmproc1 x,
	ppCurrent = xmobarColor "#89b4fa" "" . wrap "<box type=Bottom width=2 mb=2>" "</box>",
	ppVisible = xmobarColor "#cba6f7" "" . wrap "<box type=Bottom width=2 mb=2>" "</box>",
	ppHidden = xmobarColor "#45475a" "" . wrap "<box type=Bottom width=2 mb=2>" "</box>",
	ppHiddenNoWindows = xmobarColor "#313244" "",
	ppUrgent = xmobarColor "#f38ba8" "" . wrap "<box type=Bottom width=2 mb=2>" "</box>",
	ppTitle = xmobarColor "#cdd6f4" "" . shorten 60,
	ppLayout = xmobarColor "#cba6f7" "" . myLayoutPrinter,
	ppSep = "  "
	}

toggleFloat w = windows (\s ->
 	if M.member w (W.floating s)
 		then W.sink w s
		else (W.float w (W.RationalRect (1 / 3) (1 / 4) (1 / 2) (1 / 2)) s)
	)

myKeys = [
	("M-c", kill1),
	("M-<Tab>", sendMessage NextLayout),
	("M-<Up>", sendMessage MirrorExpand),
	("M-<Down>", sendMessage MirrorShrink),
	("M-<Left>", sendMessage Shrink),
	("M-<Right>", sendMessage Expand),
	("M-<Return>", spawn myTerminal),
	("M-b", spawn myBrowser),
	("M-<Space>", sendMessage $ JumpToLayout "full"),
	("M-f", withFocused toggleFloat),
	("M-t", withFocused $ windows . (flip W.float $ W.RationalRect 0 0 1 1)),
	("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute"),
	("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute"),
	("<XF86AudioMute>", spawn "amixer set Master toggle"),
	("<Print>", spawn "spectacle")
	]

main = do
	xmproc0 <- spawnPipe "xmobar -x 0 -p \"Static { xpos = 1290, ypos = 10, width = 1660, height = 24 }\" ~/.xmonad/xmobar.hs"
	xmproc1 <- spawnPipe "xmobar -x 1 -p \"Static { xpos = 10, ypos = 10, width = 1260, height = 24 }\" ~/.xmonad/xmobar.hs"
	xmonad $ ewmhFullscreen . docks $ def {
		terminal = myTerminal,
		modMask = myModMask,
		borderWidth = myBorderWidth,
		workspaces = myWorkspaces,
		manageHook = myManageHook,
		layoutHook = myLayoutHook,
		startupHook = myStartupHook,
		logHook = myLogHook xmproc0 xmproc1
	} `additionalKeysP` myKeys
