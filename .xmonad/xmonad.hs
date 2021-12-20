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

myBorderWidth = 4
myNormalColor = "#fafafa"
myFocusedColor = "#61afef"

myWorkspaces = ["main", "dev", "web", "social", "tools"]

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
        normalBorderColor = myNormalColor,
        focusedBorderColor = myFocusedColor,
        workspaces = myWorkspaces,
        manageHook = manageDocks <+> manageHook defaultConfig,
        layoutHook = myLayoutHook,
        handleEventHook = handleEventHook defaultConfig <+> docksEventHook,
        logHook = dynamicLogWithPP xmobarPP {
            ppOutput = hPutStrLn xmproc,
            ppTitle = xmobarColor "green" "" . shorten 50
        }
    } `additionalKeysP` myKeys
