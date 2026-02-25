module PipeSV.StageContext where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Language.SystemVerilog.AST

-- | Context passed down through the AST traversal.
data StageContext = StageContext
    { contextStageNames :: [String]
    , nameToIndex       :: Map.Map String Int
    , contextIndex      :: Maybe Int
    , contextLocalDecls :: Set.Set String
    }

-- | Initial context with no stages, no current index, and no local declarations.
emptyContext :: StageContext
emptyContext = StageContext [] Map.empty Nothing Set.empty

-- -------------------------------------------------------
-- CalculateContext: enriches context as we descend the AST
-- -------------------------------------------------------

-- | Traverses the AST top-down to compute and refine the context at each level.
class CalculateContext a where
    calculateContext :: StageContext -> a -> StageContext

-- | When entering a module, collects stage names in order and builds
-- the name-to-index map.
instance CalculateContext Description where
    calculateContext context (Part _ _ Module _ _ _ items) =
        let names  = [n | StageC _ (Stage n _) <- items]
            index  = Map.fromList $ zip names [0..]
        in context { contextStageNames = names, nameToIndex = index }
    calculateContext context _ = context

-- | When entering a stage, records the stage's index and the set of locally
-- declared register and wire names.
instance CalculateContext Stage where
    calculateContext context (Stage name items) =
        let index     = Map.findWithDefault 0 name (nameToIndex context)
            registers = Set.fromList [i | MIPackageItem (Decl (Variable _ _ i _ _)) <- items]
            wires     = Set.fromList [i | MIPackageItem (Decl (Net _ _ _ _ i _ _))  <- items]
        in context { contextIndex = Just index, contextLocalDecls = Set.union registers wires }

-- | Extends contextLocalDecls with the names declared in a Block's [Decl]
-- list, so that block-local variables (e.g. loop counters) are not renamed
-- by applyDefaultOffset when recursing into the block's statements.
addBlockLocals :: StageContext -> [Decl] -> StageContext
addBlockLocals context decls =
    let names = [name | Variable _ _ name _ _ <- decls]
             ++ [name | Net _ _ _ _ name _ _ <- decls]
    in context { contextLocalDecls = Set.union (contextLocalDecls context) (Set.fromList names) }
