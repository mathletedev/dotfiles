import XMonad
import XMonad.Hooks.DynamicLog

myTerminal = "kitty"
myModmask = mod4Mask
myBorderWidth = 4
myNormalBorderColor = "#ffffff"
myFocusedBorderColor = "#ffffff"
myBar = "xmobar"
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "|" "|" }
myToggleStrutsKey XConfig { XMonad.modMask = modMask } = (modMask, xK_b)

main = xmonad =<< statusBar myBar myPP myToggleStrutsKey defaults

defaults = def {
	terminal = myTerminal,
	modMask = myModmask,
	borderWidth = myBorderWidth,
	normalBorderColor = myNormalBorderColor,
	focusedBorderColor = myFocusedBorderColor
	}
