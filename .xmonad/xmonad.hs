import System.IO

import qualified Data.Map as M

import XMonad hiding ((|||))
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run

import Graphics.X11.ExtraTypes.XF86

myTerminal = "kitty"
myBrowser = "firefox-developer-edition"

myModMask = mod4Mask

myBorderWidth = 4
myFocusedBorderColor = "#373c45"
myNormalBorderColor = "#373c45"

myWorkspaces = [" <fn=1>\xf015</fn> ", " <fn=1>\xf1c9</fn> ", " <fn=1>\xf0ac</fn> ", " <fn=1>\xf4ad</fn> ", " <fn=1>\xf7d9</fn> ", " <fn=1>\xf233</fn> "]

mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

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
    renamed [Replace "full"]
    Full

myLayoutHook = avoidStruts $ tall ||| wide ||| grid ||| full

toggleFloat w = windows (\s ->
    if M.member w (W.floating s)
        then W.sink w s
        else (W.float w (W.RationalRect (1 / 3) (1 / 4) (1 / 2) (1 / 2)) s)
    )

myLayoutPrinter "tall" = "<fn=1>\xf338</fn>"
myLayoutPrinter "wide" = "<fn=1>\xf337</fn>"
myLayoutPrinter "grid" = "<fn=1>\xf424</fn>"
myLayoutPrinter "full" = "<fn=1>\xf31e</fn>"
myLayoutPrinter x = x

myKeys = [
    ("M-<Tab>", sendMessage NextLayout),
    ("M-c", kill1),
    ("M-<Up>", sendMessage MirrorExpand),
    ("M-<Down>", sendMessage MirrorShrink),
    ("M-<Left>", sendMessage Shrink),
    ("M-<Right>", sendMessage Expand),
    ("M-<Return>", spawn myTerminal),
    ("M-b", spawn myBrowser),
    ("M-<Space>", sendMessage (JumpToLayout "full")),
    ("M-t", sendMessage ToggleStruts),
    ("M-f", withFocused toggleFloat),
    ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+ unmute"),
    ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%- unmute"),
    ("<XF86AudioMute>", spawn "amixer set Master toggle")
    ]

main = do
    xmproc <- spawnPipe "xmobar"
    xmonad $ def {
        terminal = myTerminal,
        modMask = myModMask,
        borderWidth = myBorderWidth,
        focusedBorderColor = myFocusedBorderColor,
        normalBorderColor = myNormalBorderColor,
        workspaces = myWorkspaces,
        manageHook = manageDocks <+> manageHook def,
        layoutHook = myLayoutHook,
        handleEventHook = handleEventHook def <+> docksEventHook,
        logHook = dynamicLogWithPP $ xmobarPP {
            ppOutput = hPutStrLn xmproc,
            ppCurrent = xmobarColor "#61afef" "" . wrap "<box type=Bottom width=2 mb=2>" "</box>",
            ppHidden = xmobarColor "#c678dd" "" . wrap "<box type=Bottom width=2 mb=2>" "</box>",
            ppHiddenNoWindows = xmobarColor "#c678dd" "",
            ppUrgent = xmobarColor "#e06c75" "" . wrap "<box type=Bottom width=2 mb=2>" "</box>",
            ppTitle = xmobarColor "#dcdfe4" "",
            ppLayout = xmobarColor "#56b6c2" "" . myLayoutPrinter,
            ppSep = "  "
        }
    } `additionalKeysP` myKeys
