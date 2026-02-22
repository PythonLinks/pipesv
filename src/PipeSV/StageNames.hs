module PipeSV.StageNames where
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Language.SystemVerilog.AST

-- Collect all stage names from an AST in declaration order.
-- Returns:
--   [String]         -- index -> name  (look up with !!)
--   Map String Int   -- name -> index  (look up with Map.lookup or Map.!)

collectStages :: AST -> ([String], Map.Map String Int)
collectStages ast = (names, index)
    where
      names = concatMap stageNamesFromAST ast
      index = Map.fromList $ zip names [0..]

stageNamesFromAST :: Description -> [String]
stageNamesFromAST (Part _ _ _ _ _ _ items) =
      mapMaybe stageNamesFromModuleItem items
stageNamesFromAST _ = []

stageNamesFromModuleItem :: ModuleItem -> Maybe String
stageNamesFromModuleItem (StageC _ (Stage stageName _)) = Just stageName
stageNamesFromModuleItem _ = Nothing