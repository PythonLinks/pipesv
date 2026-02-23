module PipeSV.EditAst where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

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

-- | Top-level entry point. Rewrites all stage expressions in an AST.
rewriteAST :: AST -> IO AST
rewriteAST ast =
    mapM (editAST emptyContext) ast

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

-- -------------------------------------------------------
-- EditAST: rewrites stage expressions using the context
-- -------------------------------------------------------

-- | Traverses the AST, using the context to rewrite stage expressions.
-- Only a few specific cases do real work; everything else passes through unchanged.
class EditAST a where
    editAST :: StageContext -> a -> IO a

-- | Enriches the context with stage names then recurses into module items.
-- All other Description variants pass through unchanged.
instance EditAST Description where
    editAST context d@(Part attrs b kw lt name ports items) = do
        let context' = calculateContext context d
        items' <- mapM (editAST context') items
        return (Part attrs b kw lt name ports items')
    editAST _ d = return d

-- | Handles StageC (enriches context, recurses into stage body),
-- Assign (checks LHS, rewrites RHS), and Statement (recurses into stmt).
-- All other ModuleItem variants pass through unchanged.
instance EditAST ModuleItem where
    editAST context (StageC kw stage@(Stage name _)) = do
        let context' = calculateContext context stage
        stage' <- editAST context' stage
        return (StageC kw stage')
    editAST context (Assign opt lhs expr) = do
        checkLHS lhs
        expr' <- editAST context expr
        return (Assign opt lhs expr')
    editAST context (Statement stmt) = do
        stmt' <- editAST context stmt
        return (Statement stmt')
    editAST _ mi = return mi

-- | Recurses into all items in the stage body.
instance EditAST Stage where
    editAST context (Stage name items) = do
        items' <- mapM (editAST context) items
        return (Stage name items')

-- | Rewrites the RHS of assignments. Checks LHS for illegal stage expressions.
-- All other Stmt variants pass through unchanged.
instance EditAST Stmt where
    editAST context (Asgn op timing lhs expr) = do
        checkLHS lhs
        expr' <- editAST context expr
        return (Asgn op timing lhs expr')
    editAST _ stmt = return stmt

-- | Rewrites stage expressions and plain identifiers. Recurses into compound
-- expressions. All other Expr variants pass through unchanged.
instance EditAST Expr where
    -- Stage expressions: apply renaming rules
    editAST context (StageExpr se) = do
        case contextIndex context of
            Nothing -> do
                hPutStrLn stderr "Error: stage expression appears outside of a stage"
                exitFailure
            Just currentStageIndex -> editStageExpr context currentStageIndex se
    -- Plain identifiers: apply default -1 offset
    editAST context (Ident name) =
        case contextIndex context of
            Nothing -> return (Ident name)
            Just currentStageIndex ->
                if Set.member name (contextLocalDecls context)
                    then return (Ident name)
                    else applyDefaultOffset context currentStageIndex name

    -- Compound expressions: recurse into sub-expressions
    editAST context (BinOpA op attrs l r) = do
        l' <- editAST context l
        r' <- editAST context r
        return (BinOpA op attrs l' r')
    editAST context (UniOpA op attrs e) = do
        e' <- editAST context e
        return (UniOpA op attrs e')
    editAST context (MuxA attrs c t f) = do
        c' <- editAST context c
        t' <- editAST context t
        f' <- editAST context f
        return (MuxA attrs c' t' f')
    editAST context (Concat exprs) = do
        exprs' <- mapM (editAST context) exprs
        return (Concat exprs')
    editAST context (Call function args) = do
        function' <- editAST context function
        args'     <- editAST context args
        return (Call function' args')
    editAST context (Bit e index) = do
        e' <- editAST context e
        return (Bit e' index)
    editAST context (Range e mode r) = do
        e' <- editAST context e
        return (Range e' mode r)
    -- All other expressions: pass through unchanged
    editAST _ expr = return expr

instance EditAST Args where
    editAST context (Args positional keyword) = do
        positional' <- mapM (editAST context) positional
        keyword'    <- mapM (\(name, expr) -> do
                            expr' <- editAST context expr
                            return (name, expr')) keyword
        return (Args positional' keyword')

-- -------------------------------------------------------
-- Helpers
-- -------------------------------------------------------

-- | Edits a stage expression to a plain Verilog expression by renaming
-- the identifier to its target-stage name via editName.
editStageExpr :: StageContext -> Int -> StageExpression -> IO Expr

-- a(-1)      →  a_previousStageName
--            →  Ident "a_previousStageName"
editStageExpr context currentStageIndex (StageOffset ident offset) = do
    newName <- editName context currentStageIndex ident offset
    return (Ident newName)

-- a(-1)[3]   →  a_previousStageName[3]
--            →  Bit (Ident "a_previousStageName") 3
editStageExpr context currentStageIndex (StageSelect ident offset index) = do
    newName <- editName context currentStageIndex ident offset
    return (Bit (Ident newName) index)

-- a(-1)[3:0] →  a_previousStageName[3:0]
--            →  Range (Ident "a_previousStageName") mode r
editStageExpr context currentStageIndex (StageRange ident offset (mode, r)) = do
    newName <- editName context currentStageIndex ident offset
    return (Range (Ident newName) mode r)

-- | Calculates the new name of a reg or wire on the right-hand side.
-- Looks up the target stage from the offset and returns "ident_targetStageName",
-- or the original ident if the target is -1 (port input).
editName :: StageContext -> Int -> String -> Offset -> IO String
editName context currentStageIndex ident offset
    -- Variable is declared in the current stage
    | Set.member ident (contextLocalDecls context) =
        if offsetInt /= 0
            then do

                -- Non-zero offset on a local variable: error, it doesn't exist in other stages
                hPutStrLn stderr $ "Error: " ++ ident ++ " is declared in the current stage and cannot be referenced with a non-zero offset"
                exitFailure
            else do

                -- Zero offset on a local: redundant but legal, warn and continue
                hPutStrLn stderr $ "Warning: " ++ ident ++ "(0) is redundant; the variable is declared in the current stage"
                return ident

    -- Target stage is before the first stage: error
    | tgt < -1 = do
        hPutStrLn stderr $ "Error: stage offset for " ++ ident ++ " is too far back"
        exitFailure

    -- Target stage is after the last stage: error
    | tgt > lastIndex = do
        hPutStrLn stderr $ "Error: stage offset for " ++ ident ++ " is too far forward"
        exitFailure

    -- Target is -1: this is a port input, leave the name unchanged
    | tgt == -1 =
        return ident

    -- Target is a valid stage: rename to ident_stageName
    | otherwise =
        return $ ident ++ "_" ++ (contextStageNames context !! tgt)
    where
        offsetInt = offsetToInt offset
        tgt       = currentStageIndex + offsetInt
        lastIndex = length (contextStageNames context) - 1

-- | Applies the implicit default offset of -1 to a plain identifier inside
-- a stage. Returns the original name unchanged if the target is -1 (port input).
-- a  →  a_previousStageName
--    →  Ident "a_previousStageName"
applyDefaultOffset :: StageContext -> Int -> String -> IO Expr
applyDefaultOffset context currentStageIndex name
    | tgt == -1 =
        return (Ident name)
    | tgt < -1  = do
        hPutStrLn stderr $ "Error: default -1 offset out of bounds for " ++ name
        exitFailure
    | otherwise =
        return $ Ident (name ++ "_" ++ (contextStageNames context !! tgt))
    where tgt = currentStageIndex - 1

-- | Converts an Offset to a signed Int. Negative offsets refer to past stages.
-- Offset Negative 1  →  -1
-- Offset Positive 1  →   1
offsetToInt :: Offset -> Int
offsetToInt (Offset Positive n) = fromIntegral n
offsetToInt (Offset Negative n) = -(fromIntegral n)

-- | Checks that a LHS does not contain a stage expression.
-- This is a safety placeholder; LHS cannot directly contain StageExpr
-- in the current AST.
checkLHS :: LHS -> IO ()
checkLHS _ = return ()
