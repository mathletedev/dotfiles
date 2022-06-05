static int topbar = 1;
static const char *fonts[] = { "Ubuntu:size=10" };
static const char *prompt = "";
static const char *colors[SchemeLast][2] = {
	[SchemeNorm] = { "#cdd6f4", "#1e1e2e" },
	[SchemeSel] = { "#89b4fa", "#1e1e2e" }
};

static unsigned int lines = 20;
static const char worddelimiters[] = " ";
static const unsigned int border_width = 5;
