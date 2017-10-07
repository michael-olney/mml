if exists("b:current_syntax")
    finish
endif

syntax match Brace /[{}<>]/
syntax match Arrow /→/
syntax match Macro /#include/
syntax match Other /[~^]/
syntax match URL /[A-Za-z]\+:\/\/[^<>{}→~^]*/
syntax match Var /$[A-Za-z_][A-Za-z_0-9]*/

highlight link Brace SpecialChar
highlight link Arrow Operator
highlight link Other Operator
highlight link Macro Include
highlight link URL Underlined
highlight link Var Identifier
