import System.IO
import XMonad
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders
import XMonad.Layout.Renamed
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.GridVariants (Grid(Grid))
import XMonad.Layout.MultiToggle (mkToggle, single, EOT(EOT), (??))
import XMonad.Layout.MultiToggle.Instances (StdTransformers(NBFULL, NOBORDERS))
import XMonad.Layout.MultiToggle (Toggle(..))
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run

myTerminal = "kitty"

myModMask = mod4Mask

myBorderWidth = 0

myWorkspaces = [" main ", " dev ", " web ", " social ", " tools "]

mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

tall =
    renamed [Replace "tall"] $
    mySpacing 8 $
    ResizableTall 1 (3 / 100) (1 / 2) []

wide =
    renamed [Replace "wide"] $
    mySpacing 8 $
    Mirror (Tall 1 (3 / 100) (1 / 2))

grid =
    renamed [Replace "grid"] $
    mySpacing 8 $
    Grid (16 / 10)

myLayoutHook =
    avoidStruts $
    mkToggle (NBFULL ?? NOBORDERS ?? EOT) myDefaultLayout
        where
            myDefaultLayout = tall ||| wide ||| grid

myKeys = [
    ("M-<Tab>", sendMessage NextLayout),
    ("M-c", kill1),
    ("M-<Return>", spawn myTerminal),
    ("M-<Space>", sendMessage (Toggle NBFULL) >> sendMessage ToggleStruts)
    ]

main = do
    xmproc <- spawnPipe "xmobar ~/.config/xmobar/xmobarrc"
    xmonad $ defaultConfig {
        terminal = myTerminal,
        modMask = myModMask,
        borderWidth = myBorderWidth,
        workspaces = myWorkspaces,
        manageHook = manageDocks <+> manageHook defaultConfig,
        layoutHook = myLayoutHook,
        handleEventHook = handleEventHook defaultConfig <+> docksEventHook,
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
