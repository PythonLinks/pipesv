-- This function walks the list of Module Items, and creates a list of
-- stage names.

-- Collect the name from a single Stage
stageName :: Stage -> String
stageName (Stage name _) = name

collectStageNames :: [ModuleItem] -> [String]
collectStageNames items = concatMap go items
  where
    go (StageC _ (Stage name _)) = [name]
    go (MIAttr _ inner)          = go inner
    go _                         = []
