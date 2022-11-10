local install_path = vim.fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"

if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
	vim.fn.execute("!git clone https://github.com/wbthomason/packer.nvim " .. install_path)
end

local packer_group = vim.api.nvim_create_augroup("Packer", { clear = true })
vim.api.nvim_create_autocmd(
	"BufWritePost",
	{ command = "source <afile> | PackerCompile", group = packer_group, pattern = "init.lua" }
)

require("packer").startup(function(use)
	use "wbthomason/packer.nvim"

	use "andweeb/presence.nvim"
	use { "catppuccin/nvim", as = "catppuccin" }
	use "glepnir/dashboard-nvim"
	use "hrsh7th/cmp-nvim-lsp"
	use "hrsh7th/nvim-cmp"
	use "jose-elias-alvarez/null-ls.nvim"
	use "kyazdani42/nvim-web-devicons"
	use "L3MON4D3/LuaSnip"
	use "lewis6991/gitsigns.nvim"
	use "neovim/nvim-lspconfig"
	use "nvim-lua/plenary.nvim"
	use "nvim-lualine/lualine.nvim"
	use "nvim-telescope/telescope.nvim"
	use "nvim-telescope/telescope-file-browser.nvim"
	use "nvim-treesitter/nvim-treesitter"
	use "onsails/lspkind-nvim"
	use "romainl/vim-cool"
	use "ryanoasis/vim-devicons"
	use "saadparwaiz1/cmp_luasnip"
	use "simrat39/rust-tools.nvim"
	use "tpope/vim-commentary"
	use "williamboman/mason.nvim"
	use "williamboman/mason-lspconfig.nvim"
	use "windwp/nvim-autopairs"
	use "https://git.sr.ht/~whynothugo/lsp_lines.nvim"
end)
