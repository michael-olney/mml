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
0) Add tests for evaluation, traceback
1) Make attrs insensitive to ordering (Data.Map or something like that)
2) Introduce variables ==> <$> (pass env to macros, think about hygiene..)
3) Change colon to another character
4) Add version specifier to head!
5) Introduce code escaping mode
6) Handle different kinds of whitespace
-}

{-
Other TODO:
1) Make all symbol imports explicit (whitelist)
2) Allow macros to be defined by external executables
3) Build macros for building new macros - ensure this is modular!
4) Dependency analysis
-}
