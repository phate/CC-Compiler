module ClassConverter where

import ParserAbs

convert :: Program -> Program
convert (Program defs) = (Program $ cvtDefs defs)

cvtDefs :: [Def] -> [Def]
cvtDefs []            = []
cvtDefs ((CDef d):ds) = (cvtCDef d) ++ (cvtDefs ds)
cvtDefs (d:ds)        = d:(cvtDefs ds)

cvtCDef :: ClassDef -> [Def]
cvtCDef (ClassDef id decls) = undefined
  where strdef = SDef $ StrDef id (concat [ as | (CDeclA as) <- decls ])
