local luasnip = require "luasnip"

local cmp = require "cmp"
cmp.setup {
	snippet = {
		expand = function(args)
			luasnip.lsp_expand(args.body)
		end,
	},
	mapping = cmp.mapping.preset.insert {
		["<C-Space>"] = cmp.mapping.complete(),
		["<CR>"] = cmp.mapping.confirm { behavior = cmp.ConfirmBehavior.Replace, select = true },
		["<Tab>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_next_item()
			else
				fallback()
			end
		end, { "i", "s" }),
		["<S-Tab>"] = cmp.mapping(function(fallback)
			if cmp.visible() then
				cmp.select_prev_item()
			else
				fallback()
			end
		end, { "i", "s" }),
	},
	sources = { { name = "nvim_lsp" }, { name = "luasnip" } },
}

local null_ls = require "null-ls"
null_ls.setup {
	sources = {
		null_ls.builtins.diagnostics.eslint_d.with { extra_filetypes = { "astro" } },
		null_ls.builtins.formatting.autopep8,
		null_ls.builtins.formatting.eslint_d.with { extra_filetypes = { "astro" } },
		null_ls.builtins.formatting.gofmt,
		null_ls.builtins.formatting.prettierd,
		null_ls.builtins.formatting.rustfmt,
		null_ls.builtins.formatting.stylua,
	},
}

local lspconfig = require "lspconfig"

local telescope = require "telescope"
telescope.setup {
	defaults = {
		mappings = { n = { ["o"] = require("telescope.actions").select_default } },
		initial_mode = "normal",
		file_ignore_patterns = { "dist/", ".git/", "node_modules/", "target/" },
	},
	pickers = { find_files = { hidden = true } },
	extensions = { file_browser = { hidden = true } },
}
telescope.load_extension "file_browser"

require("nvim-treesitter.configs").setup {
	ensure_installed = {
		"astro",
		"bash",
		"cpp",
		"css",
		"go",
		"html",
		"lua",
		"make",
		"python",
		"rust",
		"tsx",
		"typescript",
		"yaml",
	},
	highlight = { enable = true },
}

require("rust-tools").setup {}

require("mason").setup {}

local servers = {
	"astro",
	"bashls",
	"clangd",
	"cssls",
	"gopls",
	"html",
	"pyright",
	"rust_analyzer",
	"sumneko_lua",
	"tailwindcss",
	"tsserver",
}
local has_formatter = { "astro", "gopls", "html", "rust_analyzer", "sumneko_lua", "tsserver" }

require("mason-lspconfig").setup {
	ensure_installed = servers,
	automatic_installation = true,
}

local opts = {
	on_attach = function(client, bufnr)
		vim.api.nvim_buf_set_option(bufnr, "omnifunc", "v:lua.vim.lsp.omnifunc")
		local opts = { buffer = bufnr }

		vim.keymap.set("n", "<Leader>h", vim.lsp.buf.hover, opts)
		vim.keymap.set("n", "<Leader>i", vim.lsp.buf.definition, opts)
		vim.keymap.set("n", "<Leader>r", vim.lsp.buf.rename, opts)

		local should_format = true
		for _, value in pairs(has_formatter) do
			if client.name == value then
				should_format = false
			end
		end

		if not should_format then
			client.server_capabilities.document_formatting = false
		end
	end,
	capabilities = require("cmp_nvim_lsp").update_capabilities(vim.lsp.protocol.make_client_capabilities()),
}

for _, server in pairs(servers) do
	if server == "sumneko_lua" then
		opts.settings = { Lua = { diagnostics = { globals = { "vim" } } } }
	end

	lspconfig[server].setup(opts)
end
