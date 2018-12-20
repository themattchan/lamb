-- Convert to nameless representation.
module Language.Lamb.Renamer where
import Language.Lamb.AST
import Language.Lamb.Utils


-- TODO add an error monad and do the sanity checks here.

-- ?? Where does the module sorting go ???

renameModule :: Mod RawName ann -> GensymM (Mod Name ann)
renameModule = error "todo"

renameDecl :: Decl RawName ann -> GensymM (Decl Name ann)
renameDecl = error "todo"

renameExp :: Exp RawName ann -> GensymM (Exp Name ann)
renameExp = error "todo"

renameTyp :: Typ RawName ann -> GensymM (Typ Name ann)
renameTyp = error "todo"
