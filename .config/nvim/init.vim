set clipboard=unnamedplus
set ignorecase
set lazyredraw
set number
set relativenumber
set cursorline
set tabstop=2
set shiftwidth=2

let mapleader = "\<space>"

nnoremap <leader>w <c-w>k
nnoremap <leader>a <c-w>h
nnoremap <leader>s <c-w>j
nnoremap <leader>d <c-w>l
nnoremap <leader>k :bn<cr>
nnoremap <leader>j :bp<cr>
nnoremap <leader>q :bd<cr>
nnoremap <leader>. <c-w><s-.>
nnoremap <leader>, <c-w><s-,>

nnoremap <leader>y :%y<cr>

autocmd filetype sh nnoremap <leader>e :term ./%<cr>
autocmd filetype c nnoremap <leader>b :!gcc % -o %:r<cr>
autocmd filetype c nnoremap <leader>e :term ./%:r<cr>
autocmd filetype cpp nnoremap <leader>b :!g++ % -o %:r<cr>
autocmd filetype cpp nnoremap <leader>e :term ./%:r<cr>
autocmd filetype java nnoremap <leader>b :!javac ./%<cr>
autocmd filetype java nnoremap <leader>e :term java %:r<cr>
autocmd filetype javascript nnoremap <leader>e :term node %<cr>
autocmd filetype python nnoremap <leader>e :term python3 %<cr>

autocmd TermOpen * startinsert

autocmd InsertEnter * :set norelativenumber
autocmd InsertLeave * :set relativenumber
noremap <silent><expr> j (v:count == 0 ? "gj" : "j")
noremap <silent><expr> k (v:count == 0 ? "gk" : "k")

call plug#begin()

Plug 'chriskempson/base16-vim'
Plug 'neoclide/coc.nvim'
Plug 'preservim/nerdtree'
Plug 'tpope/vim-commentary'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'ryanoasis/vim-devicons'

call plug#end()

colorscheme base16-nord
set termguicolors

inoremap <silent><expr> <c-space> coc#refresh()
inoremap <expr> <tab> pumvisible() ? "<c-n>" : "<tab>"
inoremap <expr> <s-tab> pumvisible() ? "<c-p>" : "<tab>"
command! -nargs=0 Prettier :CocCommand prettier.formatFile
autocmd BufWritePre * Prettier

nnoremap <leader>n :NERDTree<cr>

let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
