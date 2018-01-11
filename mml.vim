if exists("b:current_syntax")
    finish
endif

syntax match Brace /[{}<>]/
syntax match Arrow /→/
syntax match Other /[~^]/
syntax match URL /[A-Za-z]\+:\/\/[^<>{}→~^]*/
syntax match Var /$[A-Za-z_][A-Za-z_0-9]*/
syntax match Macro /#include/
syntax match Name /[^<>{}→~^]\+/ contained
syntax region NameRegion start='[{<]' end='\ze[<>{}→~^]' contains=Name, Brace
syntax region Comment start='\/\*' end='\*\/'

highlight link Brace SpecialChar
highlight link Arrow Operator
highlight link Other Operator
highlight link URL Underlined
highlight link Var Identifier
highlight link Macro Include
highlight link Name Type
highlight link Comment Comment

