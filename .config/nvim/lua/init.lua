local install_path = vim.fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
	vim.fn.execute("!git clone https://github.com/wbthomason/packer.nvim " .. install_path)
end

local packer_group = vim.api.nvim_create_augroup("Packer", { clear = true })
vim.api.nvim_create_autocmd("BufWritePost", { command = "source <afile> | PackerCompile", group = packer_group, pattern = "init.lua" })

require "packer".startup(function(use)
	use "wbthomason/packer.nvim"

	use "andweeb/presence.nvim"
	use { "catppuccin/nvim", as = "catppuccin" }
	use "hrsh7th/cmp-nvim-lsp"
	use "hrsh7th/nvim-cmp"
	use "L3MON4D3/LuaSnip"
	use { "lewis6991/gitsigns.nvim", requires = { "nvim-lua/plenary.nvim" } }
	use "neovim/nvim-lspconfig"
	use { "nvim-lualine/lualine.nvim", requires = { "kyazdani42/nvim-web-devicons" } }
	use "nvim-treesitter/nvim-treesitter"
	use "onsails/lspkind-nvim"
	use "preservim/nerdtree"
	use "ryanoasis/vim-devicons"
	use "saadparwaiz1/cmp_luasnip"
	use "tpope/vim-commentary"
	use "vim-scripts/auto-pairs-gentle"
	use { "williamboman/nvim-lsp-installer", requires = "neovim/nvim-lspconfig" }
end)

vim.o.clipboard = "unnamedplus"
vim.o.ignorecase = true
vim.o.lazyredraw = true
vim.o.number = true
vim.o.relativenumber = true
vim.o.shiftwidth = 2
vim.o.splitbelow = true
vim.o.tabstop = 2
vim.o.termguicolors = true
vim.o.updatetime = 100

vim.g.mapleader = " "

vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })
vim.keymap.set("n", "<Leader>w", "<C-w>k")
vim.keymap.set("n", "<Leader>a", "<C-w>h")
vim.keymap.set("n", "<Leader>s", "<C-w>j")
vim.keymap.set("n", "<Leader>d", "<C-w>l")
vim.keymap.set("n", "<Leader>j", ":bp<CR>", { silent = true })
vim.keymap.set("n", "<Leader>k", ":bn<CR>", { silent = true })
vim.keymap.set("n", "<Leader>q", ":bp<CR>:bd #<CR>", { silent = true })
vim.keymap.set("n", "<Leader>/", ":let @/ = \"\"<CR>", { silent = true })
vim.keymap.set("n", "<leader>y", ":%y<CR>")
vim.keymap.set("n", "k", "v:count == 0 ? \"gk\" : \"k\"", { expr = true, silent = true })
vim.keymap.set("n", "j", "v:count == 0 ? \"gj\" : \"j\"", { expr = true, silent = true })

local lang_maps = {
	cpp = { build = "g++ % -o %:r", exec = "./%:r" },
	javascript = { exec = "node %" },
	python = { exec = "python3 %" },
	java = { build = "javac %", exec = "java %:r" },
	sh = { exec = "./%" },
	go = { build = "go build", exec = "go run %" },
	rust = { exec = "cargo run" }
}
for lang, data in pairs(lang_maps) do
	if data.build ~= nil then vim.api.nvim_create_autocmd("FileType", { command = "nnoremap <leader>b :!" .. data.build .. "<cr>", pattern = lang }) end
	vim.api.nvim_create_autocmd("FileType", { command = "nnoremap <leader>e :sp<CR>:ter " .. data.exec .. "<cr>", pattern = lang })
end

local formatters = { "ts", "cpp", "js", "py" }
for _, formatter in pairs(formatters) do
	vim.api.nvim_create_autocmd("BufWritePre", { command = "lua vim.lsp.buf.formatting_sync(nil, 1000)", pattern = "*." .. formatter })
end

vim.api.nvim_create_autocmd("InsertEnter", { command = "set norelativenumber", pattern = "*" })
vim.api.nvim_create_autocmd("InsertLeave", { command = "set relativenumber", pattern = "*" })
vim.api.nvim_create_autocmd("TermOpen", { command = "startinsert", pattern = "*" })

require "presence":setup {
	neovim_image_text = "Neovim",
	presence_log_level = "error",
	presence_editing_text = "Editing « %s »",
	presence_file_explorer_text = "Browsing files",
	presence_reading_text = "Reading  « %s »",
	presence_workspace_text = "Working on « %s »"
}

vim.g.catppuccin_flavour = "mocha"
vim.cmd [[colorscheme catppuccin]]

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").update_capabilities(capabilities)

require "gitsigns".setup {
	signs = {
		add = { text = "+" },
		change = { text = "~" },
		delete = { text = "_" },
		topdelete = { text = "‾" },
		changedelete = { text = "~" }
	}
}

-- require "lspconfig"
local lsp_installer = require "nvim-lsp-installer"
local servers = { "tsserver", "clangd", "pyright", "sumneko_lua" }
for _, name in pairs(servers) do
	local found, server = lsp_installer.get_server(name)
	if found and not server:is_installed() then
		print("Installing " .. name)
		server:install()
	end
end
local on_attach = function(_, bufnr)
	vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
	local opts = { buffer = bufnr }
	vim.keymap.set("n", "<Leader>h", vim.lsp.buf.hover, opts)
	vim.keymap.set("n", "<Leader>i", vim.lsp.buf.definition, opts)
	vim.keymap.set("n", "<Leader>r", vim.lsp.buf.rename, opts)
	vim.keymap.set("n", "<Leader>f", vim.lsp.buf.formatting, opts)
end
local setup_server = { sumneko_lua = function(opts) opts.settings = { Lua = { diagnostics = { globals = { "vim" } } } } end }
lsp_installer.on_server_ready(function(server)
	local opts = { on_attach = on_attach, capabilities = capabilities }
	if setup_server[server.name] then setup_server[server.name](opts) end
	server:setup(opts)
end)

local cp = require("catppuccin.core.palettes.init").get_palette()
local custom_catppuccin = require "lualine.themes.catppuccin"
custom_catppuccin.normal.b.bg = cp.surface0
custom_catppuccin.normal.c.bg = cp.base
custom_catppuccin.insert.b.bg = cp.surface0
custom_catppuccin.command.b.bg = cp.surface0
custom_catppuccin.visual.b.bg = cp.surface0
custom_catppuccin.replace.b.bg = cp.surface0
custom_catppuccin.inactive.a.bg = cp.base
custom_catppuccin.inactive.b.bg = cp.base
custom_catppuccin.inactive.b.fg = cp.surface0
custom_catppuccin.inactive.c.bg = cp.base
require "lualine".setup {
	options = { theme = custom_catppuccin, component_separators = "|", section_separators = { left = "", right = "" } },
	sections = {
		lualine_a = { { "mode", separator = { left = "" }, right_padding = 2 } },
		lualine_b = { "filename", "branch", { "diff", colored = false } },
		lualine_c = {},
		lualine_x = {},
		lualine_y = { "filetype", "progress" },
		lualine_z = { { "location", separator = { right = "" }, left_padding = 2 } }
	},
	inactive_sections = { lualine_a = { "filename" }, lualine_b = {}, lualine_c = {}, lualine_x = {}, lualine_y = {}, lualine_z = {} },
	tabline = { lualine_a = { { "buffers", separator = { left = "", right = "" }, right_padding = 2, symbols = { alternate_file = "" } } } }
}

require "nvim-treesitter.configs".setup {
	ensure_installed = { "typescript", "cpp", "python", "lua" },
	highlight = { enable = true }
}

vim.g.NERDTreeShowHidden = 1
vim.g.NERDTreeWinPos = "right"
vim.g.NERDTreeDirArrowExpandable = ""
vim.g.NERDTreeDirArrowCollapsible = ""
vim.keymap.set("n", "<Leader>n", ":NERDTreeToggle<CR>", { silent = true })
vim.keymap.set("n", "<Leader>r", ":NERDTreeRefreshRoot<CR>:NERDTreeRefreshRoot<CR>", { silent = true })
vim.api.nvim_create_autocmd("BufEnter", { command = "if winnr(\"$\") == 1 && exists(\"b:NERDTree\") && b:NERDTree.isTabTree() | quit | endif", pattern = "*" })

local luasnip = require "luasnip"

local cmp = require "cmp"
cmp.setup {
	snippet = { expand = function(args) luasnip.lsp_expand(args.body) end },
	mapping = cmp.mapping.preset.insert {
		["<C-Space>"] = cmp.mapping.complete(),
		["<CR>"] = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = true },
		["<Tab>"] = cmp.mapping(function(fallback) if cmp.visible() then cmp.select_next_item() else fallback() end end, { "i", "s" }),
		["<S-Tab>"] = cmp.mapping(function(fallback) if cmp.visible() then cmp.select_prev_item() else fallback() end end, { "i", "s" })
	},
	sources = { { name = "nvim_lsp" }, { name = "luasnip" } }
}
