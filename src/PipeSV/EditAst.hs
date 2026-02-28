module PipeSV.EditAst where

import Data.List (partition)
import qualified Data.Set as Set
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

import Language.SystemVerilog.AST
import PipeSV.StageContext
import PipeSV.NameResolution
import PipeSV.GenerateVariables (generateVariables)

-- | Top-level entry point. Rewrites all stage expressions in an AST.
rewriteAST :: FilePath -> AST -> IO AST
rewriteAST sourceFile ast = do
    let context = emptyContext { fileName = sourceFile }
    ast' <- mapM (applyGenerateVariables context) ast
    mapM (editAST context) ast'

-- | Applies generateVariables to the items of a module Part.
-- All other Description variants pass through unchanged.
applyGenerateVariables :: StageContext -> Description -> IO Description
applyGenerateVariables context (Part attrs b kw lt name ports items) = do
    items' <- generateVariables context items
    return (Part attrs b kw lt name ports items')
applyGenerateVariables _ description = return description

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
    editAST context (Initial stmt) = do
        stmt' <- editAST context stmt
        return (Initial stmt')
    editAST context (Generate genItems) = do
        genItems' <- mapM (editAST context) genItems
        return (Generate genItems')
    editAST _ mi = return mi

instance EditAST GenItem where
    editAST context (GenBlock name items) = do
        items' <- mapM (editAST context) items
        return (GenBlock name items')
    editAST context (GenCase expr cases) = do
        expr'  <- editAST context expr
        cases' <- mapM (\(exprs, item) -> do
                      exprs' <- mapM (editAST context) exprs
                      item'  <- editAST context item
                      return (exprs', item')) cases
        return (GenCase expr' cases')
    editAST context (GenFor (initVar, initExpr) cond (updateVar, op, updateExpr) body) = do

        -- Add loop variables to contextLocalDecls so they are not
        -- renamed by the default -1 offset rule.
        let loopLocals = Set.fromList [initVar, updateVar]
        let context'   = context { contextLocalDecls =
                            Set.union loopLocals (contextLocalDecls context) }
        initExpr'   <- editAST context' initExpr
        cond'       <- editAST context' cond
        updateExpr' <- editAST context' updateExpr
        body'       <- editAST context' body
        return (GenFor (initVar, initExpr') cond' (updateVar, op, updateExpr') body')
    editAST context (GenIf expr thenItem elseItem) = do
        expr'     <- editAST context expr
        thenItem' <- editAST context thenItem
        elseItem' <- editAST context elseItem
        return (GenIf expr' thenItem' elseItem')
    editAST context (GenModuleItem item) = do
        item' <- editAST context item
        return (GenModuleItem item')
    editAST _ genItem = return genItem

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
    editAST context (StageExpr se) =
        case contextIndex context of
            Nothing -> do
                hPutStrLn stderr $ "Error in file " ++ fileName context
                    ++ " in the function EditAst.editAST: stage expression '"
                    ++ show (StageExpr se) ++ "' appears outside of a stage"
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
-- Stage expansion
-- -------------------------------------------------------

-- expandModuleItem and expandStage remain here rather than in a separate
-- StageExpand module because they are mutually entangled with editAST:
-- expandStage calls editAST to rewrite expressions, and the EditAST instance
-- for Description calls expandModuleItem. Separating them would require
-- passing editAST as a function parameter.

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

    -- d. Wrap statements in always @(...) begin...end
    let alwaysItems = if null stmts
                        then []
                        else [AlwaysC Always (Timing (Event (contextSensitivity context'))
                                                     (Block Seq "" [] stmts))]
    return (declarations ++ alwaysItems)

isStatement :: ModuleItem -> Bool
isStatement (Statement _) = True
isStatement _             = False
