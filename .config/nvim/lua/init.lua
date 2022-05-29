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
	use { "hrsh7th/nvim-cmp", requires = { "hrsh7th/cmp-nvim-lsp", "hrsh7th/cmp-buffer", "hrsh7th/cmp-path", "hrsh7th/cmp-cmdline" } }
	use { "lewis6991/gitsigns.nvim", requires = { "nvim-lua/plenary.nvim" } }
	use "neovim/nvim-lspconfig"
	use { "nvim-lualine/lualine.nvim", requires = { "kyazdani42/nvim-web-devicons" } }
	use "nvim-treesitter/nvim-treesitter"
	use "onsails/lspkind-nvim"
	use "preservim/nerdtree"
	use "ryanoasis/vim-devicons"
  use { "tami5/lspsaga.nvim", requires = { "neovim/nvim-lspconfig" } }
	use "tpope/vim-commentary"
	use "vim-scripts/auto-pairs-gentle"
	use { "williamboman/nvim-lsp-installer", requires = "neovim/nvim-lspconfig" }
end)

vim.o.clipboard = "unnamedplus"
vim.o.lazyredraw = true
vim.o.number = true
vim.o.relativenumber = true
vim.o.shiftwidth = 2
vim.o.tabstop = 2
vim.o.termguicolors = true
vim.o.updatetime = 100

vim.g.mapleader = " "

vim.keymap.set("n", "<leader>w", "<C-w>k")
vim.keymap.set("n", "<leader>a", "<C-w>h")
vim.keymap.set("n", "<leader>s", "<C-w>j")
vim.keymap.set("n", "<leader>d", "<C-w>l")
vim.keymap.set("n", "<leader>/", ":let @/ = \"\"<CR>", { silent = true })
vim.keymap.set("n", "k", "v:count == 0 ? \"gk\" : \"k\"", { expr = true, silent = true })
vim.keymap.set("n", "j", "v:count == 0 ? \"gj\" : \"j\"", { expr = true, silent = true })

vim.api.nvim_create_autocmd("InsertEnter", { command = "set norelativenumber", pattern = "*" })
vim.api.nvim_create_autocmd("InsertLeave", { command = "set relativenumber", pattern = "*" })

require "presence":setup {
	neovim_image_text = "Neovim",
	presence_log_level = "error",
	presence_editing_text = "Editing « %s »",
	presence_file_explorer_text = "Browsing files",
	presence_reading_text = "Reading  « %s »",
	presence_workspace_text = "Working on « %s »"
}

vim.g.catppuccin_flavour = "macchiato"
vim.cmd [[colorscheme catppuccin]]

require "gitsigns".setup {
  signs = {
    add = { text = "+" },
    change = { text = "~" },
    delete = { text = "_" },
    topdelete = { text = "‾" },
    changedelete = { text = "~" }
  }
}

require "lspconfig"
local lsp_installer = require "nvim-lsp-installer"
local servers = { "pyright", "sumneko_lua" }
for _, name in pairs(servers) do
  local found, server = lsp_installer.get_server(name)
  if found and not server:is_installed() then
    print("Installing " .. name)
    server:install()
  end
end

require "lualine".setup {
  options = {
    theme = "catppuccin"
  }
}

require "nvim-treesitter.configs".setup {
  ensure_installed = { "python", "lua" },
  highlight = { enable = true }
}

vim.g.NERDTreeShowHidden = 1
vim.g.NERDTreeWinPos = "right"
vim.g.NERDTreeDirArrowExpandable = ""
vim.g.NERDTreeDirArrowCollapsible = ""
vim.keymap.set("n", "<leader>n", ":NERDTreeToggle<CR>", { silent = true })
vim.keymap.set("n", "<leader>r", ":NERDTreeRefreshRoot<CR>:NERDTreeRefreshRoot<CR>", { silent = true })

local cmp = require "cmp"
cmp.setup {
	mapping = cmp.mapping.preset.insert {
		["<C-Space>"] = cmp.mapping.complete()
	},
	sources = { name = "nvim_lsp" }
}
