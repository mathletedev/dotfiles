import System.IO
import XMonad
import XMonad.Util.Run
import XMonad.Hooks.DynamicLog
import XMonad.Layout.LayoutModifier
import XMonad.Layout.Spacing

myTerminal = "kitty"

myModMask = mod4Mask

myBorderWidth = 4
myNormalColor = "#fafafa"
myFocusedColor = "#0997b3"

mySpacing :: Integer -> l a -> XMonad.Layout.LayoutModifier.ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border i i i i) True (Border i i i i) True

main = do
	xmproc <- spawnPipe "xmobar ~/.config/xmobar/xmobarrc"
	xmonad $ def {
		terminal = myTerminal,
		modMask = myModMask,
		borderWidth = myBorderWidth,
		normalBorderColor = myNormalColor,
		focusedBorderColor = myFocusedColor
	}
