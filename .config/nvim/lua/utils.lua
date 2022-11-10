local lang_maps = {
	cpp = { build = "g++ % -o %:r", exec = "./%:r" },
	typescript = { exec = "bun %" },
	javascript = { exec = "bun %" },
	python = { exec = "python %" },
	java = { build = "javac %", exec = "java %:r" },
	sh = { exec = "./%" },
	go = { build = "go build", exec = "go run %" },
	rust = { exec = "cargo run" },
	arduino = {
		build = "arduino-cli compile --fqbn arduino:avr:uno %:r",
		exec = "arduino-cli upload -p /dev/ttyACM0 --fqbn arduino:avr:uno %:r",
	},
}

for lang, data in pairs(lang_maps) do
	if data.build ~= nil then
		vim.api.nvim_create_autocmd(
			"FileType",
			{ command = "nnoremap <Leader>b :!" .. data.build .. "<CR>", pattern = lang }
		)
	end

	vim.api.nvim_create_autocmd(
		"FileType",
		{ command = "nnoremap <Leader>e :split<CR>:terminal " .. data.exec .. "<CR>", pattern = lang }
	)
end

vim.api.nvim_create_autocmd("BufWritePre", {
	command = "lua vim.lsp.buf.format()",
	pattern = "*.astro,*.cpp,*.css,*.go,*.h,*.html,*.js,*.json,*.jsx,*.lua,*.md,*.py,*.rs,*.ts,*.tsx,*.yaml",
})

vim.api.nvim_create_autocmd("InsertEnter", { command = "set norelativenumber", pattern = "*" })
vim.api.nvim_create_autocmd("InsertLeave", { command = "set relativenumber", pattern = "*" })

vim.api.nvim_create_autocmd("TermOpen", { command = "startinsert", pattern = "*" })

vim.api.nvim_create_autocmd("BufWinEnter", { command = "set noexpandtab tabstop=2 shiftwidth=2", pattern = "*.rs" })
vim.api.nvim_create_autocmd("BufWinEnter", { command = "set filetype=astro", pattern = "*.astro" })

vim.api.nvim_command "sign define DiagnosticSignError text=● texthl=DiagnosticSignError"
vim.api.nvim_command "sign define DiagnosticSignWarn text=● texthl=DiagnosticSignWarn"
vim.api.nvim_command "sign define DiagnosticSignInfo text=● texthl=DiagnosticSignInfo"
vim.api.nvim_command "sign define DiagnosticSignHint text=● texthl=DiagnosticSignHint"
