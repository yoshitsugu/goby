" Vim syntax file for the Goby programming language
" Language:     Goby
" Maintainer:   Goby contributors
" URL:          https://gitlab.com/yoshitsugu/goby
" File Types:   *.gb

if exists('b:current_syntax')
  finish
endif

" Comments
syntax match gobyComment /#.*$/ contains=gobyTodo
syntax keyword gobyTodo TODO FIXME XXX NOTE contained

" Strings with escape sequences
syntax region gobyString start=/"/ skip=/\\./ end=/"/ contains=gobyEscape
syntax match gobyEscape /\\[nrt\\"']/ contained

" Integer literals
syntax match gobyNumber /\<[0-9]\+\>/

" Boolean constants
syntax keyword gobyBoolean True False

" Control-flow and effect-application keywords
syntax keyword gobyKeyword if else case with with_handler in resume can

" Declaration and module keywords
syntax keyword gobyKeyword type effect handler import as mut
syntax match gobyKeyword /@embed\>/

" Built-in type names (before gobyType to take precedence)
syntax keyword gobyBuiltin Int String Bool Unit List

" User-defined type / constructor names (UpperCamelCase)
syntax match gobyType /\<[A-Z][A-Za-z0-9_]*\>/

" Operators
syntax match gobyOperator /->/
syntax match gobyOperator /|>/
syntax match gobyOperator /==/
syntax match gobyOperator /=/
syntax match gobyOperator /+/
syntax match gobyOperator /\*/
syntax match gobyOperator /|/
syntax match gobyOperator /:/

" Highlight links — map to standard Vim highlight groups
highlight default link gobyComment   Comment
highlight default link gobyTodo      Todo
highlight default link gobyString    String
highlight default link gobyEscape    SpecialChar
highlight default link gobyNumber    Number
highlight default link gobyBoolean   Boolean
highlight default link gobyKeyword   Keyword
highlight default link gobyBuiltin   Type
highlight default link gobyType      Type
highlight default link gobyOperator  Operator

let b:current_syntax = 'goby'
