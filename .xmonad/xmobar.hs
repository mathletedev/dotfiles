Config {
	font = "xft:Ubuntu:weight=bold:pixelsize=12:antialias=true:hinting=true",
	additionalFonts = ["xft:Font Awesome 6 Free Solid:pixelsize=12"]
	position = Static { xpos = 10 , ypos = 10, width = 1660, height = 24 },
	bgColor = "#1e1e2e",
	lowerOnStart = True,
	iconRoot = ".xmonad/icons/",
	commands = [
		Run StdinReader,
		Run Date "<fc=#89b4fa><fn=1>\xf017</fn>  %H:%M:%S</fc>" "time" 10,
		Run Cpu ["-t", "<fc=#f38ba8><fn=1>\xf2db</fn>  <total>%</fc>"] 20,
		Run Memory ["-t", "<fc=#fab387><fn=1>\xf538</fn>  <usedratio>%</fc>"] 20,
		Run Alsa "default" "Master" ["-t", "<fc=#f9e2af><fn=1><status></fn>  <volume>%</fc>", "--", "-O", "\xf028", "-o", "\xf6a9", "-C", "#f9e2af", "-c", "#f9e2af"],
		Run WeatherX "KPUW" [
			("clear", "\xf185"),
			("sunny", "\xf185"),
			("fair", "\xf186"),
			("mostly clear", "\xf6c4"),
			("mostly sunny", "\xf6c4"),
			("partly cloudy", "\xf6c3"),
			("partly sunny", "\xf6c3"),
			("mostly cloudy", "\xf73c"),
			("cloudy", "\xf0c2"),
			("considerable cloudiness", "\xf740"),
			("overcast", "\xf73d"),
			("obscured", "\xf75f")
		] ["-t", "<fc=#a6e3a1><fn=1>\xf2c9</fn>  <tempC>°C  <fn=1><skyConditionS></fn></fc>"] 18000,
		Run Date "<fc=#89b4fa><fn=1>\xf133</fn>  %m/%d/%Y</fc>" "date" 36000,
		Run DynNetwork ["-t", "<fc=#cba6f7><fn=1>\xf019</fn>  <rx> KB/s  <fn=1>\xf7c0</fn>  <tx> KB/s</fc>"] 20
	],
	sepChar = "%",
	alignSep = "}{",
	template = "  <action='dmenu_run'><icon=sharingan.xpm/></action>  %StdinReader%}<action='kitty -e tty-clock -c'>%time%</action>{<action='kitty -e htop'>%cpu%</action>    %memory%    <action='kitty -e cava'>%alsa:default:Master%</action>    %KPUW%    <action='kitty --hold -e cal -y'>%date%</action>    <action='kitty -e nmtui'>%dynnetwork%</action>    "
}
