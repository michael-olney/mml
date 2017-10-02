module MML.Format where

data Format = MML | MMLPure | HTML | JSON
    deriving (Eq, Ord, Show, Read)

