set clipboard=unnamedplus
set ignorecase
set lazyredraw
set number
set relativenumber
set cursorline
set tabstop=2
set shiftwidth=2
set hidden
set splitbelow

let mapleader = "\<space>"

nnoremap <leader>w <c-w>k
nnoremap <leader>a <c-w>h
nnoremap <leader>s <c-w>j
nnoremap <leader>d <c-w>l
nnoremap <silent> <leader>k :bn<cr>
nnoremap <silent> <leader>j :bp<cr>
nnoremap <silent> <leader>q :bp<cr>:bd #<cr>
nnoremap <leader>. <c-w><s-.>
nnoremap <leader>, <c-w><s-,>

nnoremap <silent> <leader>/ :let @/ = ""<cr>
nnoremap <leader>y :%y<cr>

autocmd filetype javascript nnoremap <leader>e :sp<cr>:ter node %<cr>
autocmd filetype cpp nnoremap <leader>b :!g++ % -o %:r<cr>
autocmd filetype cpp nnoremap <leader>e :sp<cr>:ter ./%:r<cr>
autocmd filetype java nnoremap <leader>b :!javac ./%<cr>
autocmd filetype java nnoremap <leader>e :sp<cr>:ter java %:r<cr>
autocmd filetype python nnoremap <leader>e :term python3 %<cr>
autocmd filetype sh nnoremap <leader>e :sp<cr>:ter ./%<cr>
autocmd filetype go nnoremap <leader>b :!go build %<cr>
autocmd filetype go nnoremap <leader>e :sp<cr>:ter go run %<cr>
autocmd filetype rust nnoremap <leader>e :sp<cr>:ter cargo run<cr>
autocmd filetype c nnoremap <leader>b :!gcc % -o %:r<cr>
autocmd filetype c nnoremap <leader>e :sp<cr>:ter ./%:r<cr>
autocmd filetype arduino nnoremap <leader>b :!arduino-cli compile --fqbn arduino:avr:uno %:r<cr>
autocmd filetype arduino nnoremap <leader>e :!arduino-cli upload -p /dev/ttyACM0 --fqbn arduino:avr:uno %:r<cr>

autocmd filetype rust autocmd bufwinenter * set noexpandtab tabstop=2 shiftwidth=2
autocmd filetype haskell setlocal expandtab tabstop=4 shiftwidth=4 softtabstop=4

autocmd insertenter * set norelativenumber
autocmd insertleave * set relativenumber

noremap <expr> j (v:count == 0 ? "gj" : "j")
noremap <expr> k (v:count == 0 ? "gk" : "k")

autocmd termopen * startinsert

call plug#begin('~/.config/nvim/plugged')

Plug 'andweeb/presence.nvim'
Plug 'neoclide/coc.nvim'
Plug 'preservim/nerdtree'
Plug 'rakr/vim-one'
Plug 'tpope/vim-commentary'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

Plug 'ryanoasis/vim-devicons'

call plug#end()

let g:presence_neovim_image_text = "Neovim"
let g:presence_log_level = "error"
let g:presence_editing_text = "Editing « %s »"
let g:presence_file_explorer_text = "Browsing files"
let g:presence_reading_text = "Reading  « %s »"
let g:presence_workspace_text = "Working on « %s »"

let g:coc_global_extensions = ["coc-tsserver", "coc-java", "coc-go", "coc-rust-analyzer", "coc-python", "coc-html", "coc-css", "coc-prettier", "coc-pairs"]
autocmd bufwritepre * silent call CocAction('runCommand', 'editor.action.organizeImport')
nnoremap <silent> <leader>h :call CocActionAsync('doHover')<cr>
inoremap <expr> <c-space> coc#refresh()
inoremap <expr> <cr> pumvisible() ? coc#_select_confirm() : "\<C-g>u\<cr>"
inoremap <expr> <tab> pumvisible() ? "<c-n>" : "<tab>"
inoremap <expr> <s-tab> pumvisible() ? "<c-p>" : "<tab>"

let NERDTreeShowHidden = 1
let NERDTreeIgnore = ['^.git$', '^node_modules$', '^yarn.lock$', '^.next$']
let g:NERDTreeWinPos = "right"
let g:NERDTreeDirArrowExpandable = "+"
let g:NERDTreeDirArrowCollapsible = "~"
autocmd bufenter * if winnr('$') == 1 && exists('b:NERDTree') && b:NERDTree.isTabTree() | quit | endif
nnoremap <silent> <leader>n :NERDTreeToggle<cr>
nnoremap <silent> <leader>r :NERDTreeRefreshRoot<cr>:NERDTreeRefreshRoot<cr>

colorscheme one
set termguicolors

let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
let g:airline_theme="onedark"
