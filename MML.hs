module MML (
    module MML.Parse,
    module MML.Unparse,
    module MML.Eval,
    module MML.Types,
    module MML.HTML
    ) where

import MML.Eval
import MML.Parse
import MML.Unparse
import MML.Types
import MML.HTML

{-
Syntax TODO:
-1) Eliminate automatic string collapsing
0) Make attrs insensitive to ordering (Data.Map or something like that)
1) Do parsing of attribute keys with Str
2) Change {} to <%>
3) Introduce variables ==> <$> (pass env to macros, think about hygiene..)
4) Change colon to another character
5) Add version specifier to head!
6) Allow empty strings to be expressed
7) Introduce code escaping mode
8) Handle different kinds of whitespace
-}

{-
Other TODO:
1) Make all symbol imports explicit (whitelist)
2) Allow macros to be defined by external executables
3) Improve traceback for failed macros
4) Build macros for building new macros - ensure this is modular!
5) Dependency analysis
-}
