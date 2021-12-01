set clipboard=unnamedplus
set ignorecase
set lazyredraw
set number
set relativenumber
set cursorline
set tabstop=2
set shiftwidth=2
set hidden

let mapleader = "\<space>"

nnoremap <leader>w <c-w>k
nnoremap <leader>a <c-w>h
nnoremap <leader>s <c-w>j
nnoremap <leader>d <c-w>l
nnoremap <silent> <leader>k :bn<cr>
nnoremap <silent> <leader>j :bp<cr>
nnoremap <silent> <leader>q :bd<cr>
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

autocmd insertenter * :set norelativenumber
autocmd insertleave * :set relativenumber

noremap <expr> j (v:count == 0 ? "gj" : "j")
noremap <expr> k (v:count == 0 ? "gk" : "k")

autocmd termopen * startinsert

call plug#begin()

Plug 'neoclide/coc.nvim'
Plug 'preservim/nerdtree'
Plug 'rakr/vim-one'
Plug 'tpope/vim-commentary'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

Plug 'ryanoasis/vim-devicons'

call plug#end()

let g:coc_global_extensions = ['coc-pairs', 'coc-tsserver', 'coc-prettier']
autocmd bufwritepre * silent call CocAction('runCommand', 'editor.action.organizeImport')
inoremap <expr> <c-space> coc#refresh()
inoremap <expr> <cr> pumvisible() ? coc#_select_confirm() : "\<C-g>u\<cr>"
inoremap <expr> <tab> pumvisible() ? "<c-n>" : "<tab>"
inoremap <expr> <s-tab> pumvisible() ? "<c-p>" : "<tab>"
nnoremap <silent> <leader>h :call CocAction('doHover')<cr>

let NERDTreeShowHidden=1
nnoremap <leader>n :NERDTree<cr>

colorscheme one
set termguicolors

let g:airline#extensions#tabline#enabled = 1
let g:airline_powerline_fonts = 1
