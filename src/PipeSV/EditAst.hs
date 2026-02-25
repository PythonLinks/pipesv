module PipeSV.EditAst where

import Data.List (partition)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

import Language.SystemVerilog.AST
import PipeSV.GenerateVariables (generateVariables)

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
rewriteAST ast = do
    ast' <- mapM applyGenerateVariables ast
    mapM (editAST emptyContext) ast'

-- | Applies generateVariables to the items of a module Part.
-- All other Description variants pass through unchanged.
applyGenerateVariables :: Description -> IO Description
applyGenerateVariables (Part attrs b kw lt name ports items) = do
    items' <- generateVariables items
    return (Part attrs b kw lt name ports items')
applyGenerateVariables description = return description

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

-- | Enriches the context with stage names then expands module items.
-- All other Description variants pass through unchanged.
instance EditAST Description where
    editAST context d@(Part attrs b kw lt name ports items) = do
        let context' = calculateContext context d
        items' <- fmap concat $ mapM (expandModuleItem context') items
        return (Part attrs b kw lt name ports items')
    editAST _ d = return d

-- | Handles Assign, Statement, and AlwaysC (recurses into stmt).
-- All other ModuleItem variants pass through unchanged.
-- StageC is handled by expandModuleItem, which expands it into multiple items.
instance EditAST ModuleItem where
    editAST context (Assign opt lhs expr) = do
        expr' <- editAST context expr
        return (Assign opt lhs expr')
    editAST context (Statement stmt) = do
        stmt' <- editAST context stmt
        return (Statement stmt')
    editAST context (AlwaysC kw stmt) = do
        stmt' <- editAST context stmt
        return (AlwaysC kw stmt')
    editAST _ mi = return mi

-- | Rewrites assignments (renames LHS root, rewrites RHS) and recurses into
-- all compound statement variants so that identifiers nested inside explicit
-- always blocks are renamed by the same rules as top-level stage statements.
instance EditAST Stmt where
    editAST context (Asgn op timing lhs expr) = do
        lhs'  <- renameLHS context lhs
        expr' <- editAST context expr
        return (Asgn op timing lhs' expr')
    editAST context (Block kind name decls stmts) = do
        let context' = addBlockLocals context decls
        stmts' <- mapM (editAST context') stmts
        return (Block kind name decls stmts')
    editAST context (If check cond thenStmt elseStmt) = do
        cond'     <- editAST context cond
        thenStmt' <- editAST context thenStmt
        elseStmt' <- editAST context elseStmt
        return (If check cond' thenStmt' elseStmt')
    editAST context (For inits cond updates body) = do
        cond' <- editAST context cond
        body' <- editAST context body
        return (For inits cond' updates body')
    editAST context (While cond body) = do
        cond' <- editAST context cond
        body' <- editAST context body
        return (While cond' body')
    editAST context (RepeatL count body) = do
        count' <- editAST context count
        body'  <- editAST context body
        return (RepeatL count' body')
    editAST context (DoWhile cond body) = do
        cond' <- editAST context cond
        body' <- editAST context body
        return (DoWhile cond' body')
    editAST context (Forever body) = do
        body' <- editAST context body
        return (Forever body')
    editAST context (Foreach name dims body) = do
        body' <- editAST context body
        return (Foreach name dims body')
    editAST context (Timing timing body) = do
        body' <- editAST context body
        return (Timing timing body')
    editAST context (Wait expr body) = do
        expr' <- editAST context expr
        body' <- editAST context body
        return (Wait expr' body')
    editAST context (StmtAttr attr body) = do
        body' <- editAST context body
        return (StmtAttr attr body')
    editAST context (Return expr) = do
        expr' <- editAST context expr
        return (Return expr')
    editAST context (Subroutine expr args) = do
        expr' <- editAST context expr
        args' <- editAST context args
        return (Subroutine expr' args')
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
        args' <- editAST context args
        return (Call function args')
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

-- | Extends contextLocalDecls with the names declared in a Block's [Decl]
-- list, so that block-local variables (e.g. loop counters) are not renamed
-- by applyDefaultOffset when recursing into the block's statements.
addBlockLocals :: StageContext -> [Decl] -> StageContext
addBlockLocals context decls =
    let names = [name | Variable _ _ name _ _ <- decls]
             ++ [name | Net _ _ _ _ name _ _ <- decls]
    in context { contextLocalDecls = Set.union (contextLocalDecls context) (Set.fromList names) }

-- | Expands a StageC into its constituent module items; passes all other
-- items through unchanged.
expandModuleItem :: StageContext -> ModuleItem -> IO [ModuleItem]
expandModuleItem context (StageC kw stage) = do
    let Stage name _ = stage
    expanded <- expandStage context stage
    return [StageC kw (Stage name expanded)]
expandModuleItem context item = do
    item' <- editAST context item
    return [item']

-- | Expands a Stage into declarations followed by an always block.
-- Rewrites stage expressions before partitioning, so that all StageExpr
-- nodes are resolved before the items move into standard Verilog AST nodes.
expandStage :: StageContext -> Stage -> IO [ModuleItem]
expandStage context stage = do

    -- a. Enrich context with this stage's index and local declarations
    let context' = calculateContext context stage
    let Stage _ items = stage

    -- b. Rewrite stage expressions in all items first
    rewrittenItems <- mapM (editAST context') items

    -- c. Partition into statements and declarations
    let (statementItems, declarations) = partition isStatement rewrittenItems
    let stmts = [stmt | Statement stmt <- statementItems]

    -- d. Wrap statements in always @(posedge clock) begin...end
    let clockEvent  = Event (EventExpr (EventExprEdge Posedge (Ident "clock")))
    let alwaysItems = if null stmts
                        then []
                        else [AlwaysC Always (Timing clockEvent (Block Seq "" [] stmts))]
    return (declarations ++ alwaysItems)

isStatement :: ModuleItem -> Bool
isStatement (Statement _) = True
isStatement _             = False

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
-- Dispatches to editNameByIndex after resolving the StageIdentifier to a target index.
editName :: StageContext -> Int -> String -> StageIdentifier -> IO String
editName context currentStageIndex ident (StageByOffset offset) =
    editNameByIndex context currentStageIndex ident
        (currentStageIndex + offsetToInt offset)
editName context _ ident (StageByName name) =
    case Map.lookup name (nameToIndex context) of
        Nothing ->
            do hPutStrLn stderr $ "Error in EditAst.editName: unknown stage name '"
                    ++ name ++ "' in expression " ++ ident ++ "#{" ++ name ++ "}"
               exitFailure
        Just targetIndex -> editNameByIndex context targetIndex ident targetIndex

-- | Handles bounds checking and renaming once the target stage index is known.
-- Returns "ident_stageName" for valid stages, or the original ident for index -1 (port input).
editNameByIndex :: StageContext -> Int -> String -> Int -> IO String
editNameByIndex context currentStageIndex ident targetIndex
    -- Variable is declared in the current stage
    | Set.member ident (contextLocalDecls context) =
        if targetIndex /= currentStageIndex
            then do
                -- Reference to a local variable from another stage: error
                hPutStrLn stderr $ "Error in EditAst.editNameByIndex: "
                    ++ ident ++ " is declared in the current stage and cannot be referenced from another stage"
                exitFailure
            else do
                -- Same-stage reference to a local: redundant but legal, warn and continue
                hPutStrLn stderr $ "Warning: " ++ ident ++ " with same-stage reference is redundant"
                return ident

    -- Target stage is before the first stage: error
    | targetIndex < -1 = do
        hPutStrLn stderr $ "Error in EditAst.editNameByIndex: stage reference for " ++ ident ++ " is too far back"
        exitFailure

    -- Target stage is after the last stage: error
    | targetIndex > lastIndex = do
        hPutStrLn stderr $ "Error in EditAst.editNameByIndex: stage reference for " ++ ident ++ " is too far forward"
        exitFailure

    -- Target is -1: this is a port input, leave the name unchanged
    | targetIndex == -1 =
        return ident

    -- Target is a valid stage: rename to ident_stageName
    | otherwise =
        return $ ident ++ "_" ++ (contextStageNames context !! targetIndex)
    where lastIndex = length (contextStageNames context) - 1

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

-- | Renames the root identifier(s) of an LHS to ident_stageName when inside
-- a stage. Local declarations (counter, etc.) are left unchanged.
renameLHS :: StageContext -> LHS -> IO LHS
renameLHS context lhs =
    case contextIndex context of
        Nothing -> return lhs
        Just currentStageIndex ->
            let stageName = contextStageNames context !! currentStageIndex
            in renameRoot stageName lhs
  where
    renameRoot stageName (LHSIdent name)
        | Set.member name (contextLocalDecls context) = return (LHSIdent name)
        | otherwise = return (LHSIdent (name ++ "_" ++ stageName))
    renameRoot stageName (LHSBit   lhs e)     = do lhs' <- renameRoot stageName lhs; return (LHSBit lhs' e)
    renameRoot stageName (LHSRange lhs m r)   = do lhs' <- renameRoot stageName lhs; return (LHSRange lhs' m r)
    renameRoot stageName (LHSDot   lhs x)     = do lhs' <- renameRoot stageName lhs; return (LHSDot lhs' x)
    renameRoot stageName (LHSConcat lhss)     = do lhss' <- mapM (renameRoot stageName) lhss; return (LHSConcat lhss')
    renameRoot stageName (LHSStream o e lhss) = do lhss' <- mapM (renameRoot stageName) lhss; return (LHSStream o e lhss')
