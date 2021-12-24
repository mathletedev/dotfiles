import System.IO

import XMonad hiding ((|||))
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutModifier
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle (Toggle(..))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, NOBORDERS))
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run

import Graphics.X11.ExtraTypes.XF86

myTerminal = "kitty"
myBrowser = "firefox"

myModMask = mod4Mask

myBorderWidth = 0

myWorkspaces = [" main ", " dev ", " web ", " com ", " util ", " host "]

mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True

full =
    renamed [Replace "full"]
    Full

tall =
    renamed [Replace "tall"] $
    mySpacing 8 $
    -- smartSpacingWithEdge 8 $
    ResizableTall 1 (3 / 100) (1 / 2) []

wide =
    renamed [Replace "wide"] $
    mySpacing 8 $
    -- smartSpacingWithEdge 8 $
    Mirror (Tall 1 (3 / 100) (1 / 2))

grid =
    renamed [Replace "grid"] $
    mySpacing 8 $
    -- smartSpacingWithEdge 8 $
    Grid (16 / 10)

myLayoutHook = avoidStruts $ full ||| tall ||| wide ||| grid

myKeys = [
    ("M-<Tab>", sendMessage NextLayout),
    ("M-c", kill1),
    ("M-<Return>", spawn myTerminal),
    ("M-b", spawn myBrowser),
    ("M-<Space>", sendMessage $ JumpToLayout "full"),
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
        workspaces = myWorkspaces,
        manageHook = manageDocks <+> manageHook def,
        layoutHook = myLayoutHook,
        handleEventHook = handleEventHook def <+> docksEventHook,
        logHook = dynamicLogWithPP $ xmobarPP {
            ppOutput = hPutStrLn xmproc,
            ppCurrent = xmobarColor "#56b6c2" "" . wrap "<box type=Bottom width=2 mb=2 color=#56b6c2>" "</box>",
            ppHidden = xmobarColor "#c678dd" "" . wrap "<box type=Bottom width=2 mb=2 color=#c678dd>" "</box>",
            ppHiddenNoWindows = xmobarColor "#c678dd" "",
            ppUrgent = xmobarColor "#e06c75" "" . wrap "!" "!",
            ppTitle = xmobarColor "#dcdfe4" "",
            ppSep = "<fc=#98c379> | </fc>"
        }
    } `additionalKeysP` myKeys
