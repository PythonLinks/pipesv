module PipeSV.GenerateVariables where

import Control.Monad (foldM)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

import Language.SystemVerilog.AST

-- -------------------------------------------------------
-- Types
-- -------------------------------------------------------

-- | Location where a variable was declared — used in error messages.
data Location
    = ModuleScope
    | StageScope Identifier

type SymbolTable = Map.Map Identifier (Type, Location, Expr)

-- -------------------------------------------------------
-- LHS root name extraction
-- -------------------------------------------------------

-- | Returns the root variable name(s) from an LHS expression.
-- Strips index, range, and dot wrappers to reach the underlying identifier(s).
lhsRootNames :: LHS -> [Identifier]
lhsRootNames (LHSIdent name)      = [name]
lhsRootNames (LHSBit   lhs _)     = lhsRootNames lhs
lhsRootNames (LHSRange lhs _ _)   = lhsRootNames lhs
lhsRootNames (LHSDot   lhs _)     = lhsRootNames lhs
lhsRootNames (LHSConcat lhss)     = concatMap lhsRootNames lhss
lhsRootNames (LHSStream _ _ lhss) = concatMap lhsRootNames lhss

-- -------------------------------------------------------
-- Statement traversal
-- -------------------------------------------------------

-- | Collects all root variable names assigned anywhere in a statement tree.
stmtAssignedNames :: Stmt -> [Identifier]
stmtAssignedNames (Asgn _ _ lhs _)           = lhsRootNames lhs
stmtAssignedNames (Block _ _ _ stmts)        = concatMap stmtAssignedNames stmts
stmtAssignedNames (If _ _ thenStmt elseStmt) = stmtAssignedNames thenStmt
                                            ++ stmtAssignedNames elseStmt
stmtAssignedNames (Case _ _ _ cases)         = concatMap (stmtAssignedNames . snd) cases
stmtAssignedNames (For _ _ _ stmt)           = stmtAssignedNames stmt
stmtAssignedNames (While _ stmt)             = stmtAssignedNames stmt
stmtAssignedNames (RepeatL _ stmt)           = stmtAssignedNames stmt
stmtAssignedNames (DoWhile _ stmt)           = stmtAssignedNames stmt
stmtAssignedNames (Forever stmt)             = stmtAssignedNames stmt
stmtAssignedNames (Foreach _ _ stmt)         = stmtAssignedNames stmt
stmtAssignedNames (Timing _ stmt)            = stmtAssignedNames stmt
stmtAssignedNames (Wait _ stmt)              = stmtAssignedNames stmt
stmtAssignedNames (StmtAttr _ stmt)          = stmtAssignedNames stmt
stmtAssignedNames _                          = []

-- -------------------------------------------------------
-- Identifier substitution
-- -------------------------------------------------------

-- | Applies a name substitution map to an expression, replacing Ident nodes
-- whose name appears in the map.
substituteInExpr :: Map.Map String String -> Expr -> Expr
substituteInExpr subst (Ident name) =
    Ident (Map.findWithDefault name name subst)
substituteInExpr subst (BinOpA op attrs l r) =
    BinOpA op attrs (substituteInExpr subst l) (substituteInExpr subst r)
substituteInExpr subst (UniOpA op attrs e) =
    UniOpA op attrs (substituteInExpr subst e)
substituteInExpr subst (MuxA attrs c t f) =
    MuxA attrs (substituteInExpr subst c)
               (substituteInExpr subst t)
               (substituteInExpr subst f)
substituteInExpr subst (Concat exprs) =
    Concat (map (substituteInExpr subst) exprs)
substituteInExpr subst (Call name args) =
    Call name (substituteInArgs subst args)
substituteInExpr subst (Bit e index) =
    Bit (substituteInExpr subst e) index
substituteInExpr subst (Range e mode r) =
    Range (substituteInExpr subst e) mode r
substituteInExpr _ expr = expr

-- | Applies a name substitution map to a function call argument list.
substituteInArgs :: Map.Map String String -> Args -> Args
substituteInArgs subst (Args positional keyword) =
    Args (map (substituteInExpr subst) positional)
         (map (\(name, expr) -> (name, substituteInExpr subst expr)) keyword)

-- | Applies a name substitution map to an LHS expression.
substituteInLHS :: Map.Map String String -> LHS -> LHS
substituteInLHS subst (LHSIdent name) =
    LHSIdent (Map.findWithDefault name name subst)
substituteInLHS subst (LHSBit lhs e) =
    LHSBit (substituteInLHS subst lhs) e
substituteInLHS subst (LHSRange lhs mode range) =
    LHSRange (substituteInLHS subst lhs) mode range
substituteInLHS subst (LHSDot lhs field) =
    LHSDot (substituteInLHS subst lhs) field
substituteInLHS subst (LHSConcat lhss) =
    LHSConcat (map (substituteInLHS subst) lhss)
substituteInLHS subst (LHSStream op e lhss) =
    LHSStream op e (map (substituteInLHS subst) lhss)

-- | Applies a name substitution map throughout a statement tree.
substituteInStmt :: Map.Map String String -> Stmt -> Stmt
substituteInStmt subst (Asgn op timing lhs expr) =
    Asgn op timing (substituteInLHS subst lhs) (substituteInExpr subst expr)
substituteInStmt subst (Block kind name decls stmts) =
    Block kind name decls (map (substituteInStmt subst) stmts)
substituteInStmt subst (If check cond thenStmt elseStmt) =
    If check (substituteInExpr subst cond)
             (substituteInStmt subst thenStmt)
             (substituteInStmt subst elseStmt)
substituteInStmt subst (Case check kind expr cases) =
    Case check kind (substituteInExpr subst expr)
         [(map (substituteInExpr subst) exprs, substituteInStmt subst stmt)
         | (exprs, stmt) <- cases]
substituteInStmt subst (For inits cond updates body) =
    For [(substituteInLHS subst lhs, substituteInExpr subst e) | (lhs, e) <- inits]
        (substituteInExpr subst cond)
        [(substituteInLHS subst lhs, op, substituteInExpr subst e) | (lhs, op, e) <- updates]
        (substituteInStmt subst body)
substituteInStmt subst (While cond body) =
    While (substituteInExpr subst cond) (substituteInStmt subst body)
substituteInStmt subst (RepeatL count body) =
    RepeatL (substituteInExpr subst count) (substituteInStmt subst body)
substituteInStmt subst (DoWhile cond body) =
    DoWhile (substituteInExpr subst cond) (substituteInStmt subst body)
substituteInStmt subst (Forever body) =
    Forever (substituteInStmt subst body)
substituteInStmt subst (Foreach name dims body) =
    Foreach name dims (substituteInStmt subst body)
substituteInStmt subst (Timing timing body) =
    Timing timing (substituteInStmt subst body)
substituteInStmt subst (Return expr) =
    Return (substituteInExpr subst expr)
substituteInStmt subst (Subroutine expr args) =
    Subroutine (substituteInExpr subst expr) (substituteInArgs subst args)
substituteInStmt subst (Force blocking lhs expr) =
    Force blocking (substituteInLHS subst lhs) (substituteInExpr subst expr)
substituteInStmt subst (Wait expr body) =
    Wait (substituteInExpr subst expr) (substituteInStmt subst body)
substituteInStmt subst (StmtAttr attr body) =
    StmtAttr attr (substituteInStmt subst body)
substituteInStmt _ stmt = stmt

-- | Applies a name substitution map to a Statement or AlwaysC module item.
-- All other module item variants pass through unchanged.
substituteInModuleItem :: Map.Map String String -> ModuleItem -> ModuleItem
substituteInModuleItem subst (Statement stmt) =
    Statement (substituteInStmt subst stmt)
substituteInModuleItem subst (AlwaysC kw stmt) =
    AlwaysC kw (substituteInStmt subst stmt)
substituteInModuleItem _ item = item

-- | Renames the variable name in a local Variable declaration using the
-- substitution map, preserving direction, type, ranges, and initializer.
-- All other module item variants pass through unchanged.
renameLocalDecl :: Map.Map String String -> ModuleItem -> ModuleItem
renameLocalDecl subst (MIPackageItem (Decl (Variable dir declType name ranges initializer))) =
    MIPackageItem (Decl (Variable dir declType
        (Map.findWithDefault name name subst) ranges initializer))
renameLocalDecl _ item = item

-- -------------------------------------------------------
-- Symbol table initialization
-- -------------------------------------------------------

-- | Builds the initial symbol table from module-scope declarations.
-- Stage items are ignored — they are added later by processStage.
initSymbolTable :: [ModuleItem] -> SymbolTable
initSymbolTable items = Map.fromList $ concatMap extractDecl items
  where

    extractDecl (MIPackageItem (Decl (Variable _ declType name _ initializer))) =
        [(name, (declType, ModuleScope, initializer))]

    extractDecl (MIPackageItem (Decl (Net _ _ _ declType name _ initializer))) =
        [(name, (declType, ModuleScope, initializer))]

    extractDecl _ = []

-- -------------------------------------------------------
-- Stage processing
-- -------------------------------------------------------

-- | Processes a single stage: renames stage-local declarations to
-- name_stageName (preserving their initializer), substitutes all references
-- to those names throughout the stage's statements and always blocks, and
-- generates pipeline register declarations (name_stageName = 0) for each
-- module-scope variable assigned in the stage.
processStage :: SymbolTable -> String -> [ModuleItem] -> IO ([ModuleItem], SymbolTable)
processStage symbolTable stageName items = do

    -- Step 1: Add stage-local declarations to the symbol table,
    -- checking for duplicates, and collect their names.
    (stageTable, localDeclNames) <- foldM addDecl (symbolTable, []) declItems

    -- Step 2: Build the substitution map: rename every stage-local variable
    -- from 'name' to 'name_stageName' in declarations and all references.
    let localRenaming = Map.fromList
            [(name, name ++ "_" ++ stageName) | name <- localDeclNames]

    -- Step 3: Collect LHS root names from all assignment statements, then
    -- identify module-scope variables (not covered by local declarations).
    let assignedNames    = nub $ concatMap stmtAssignedNames stmts
    let newAssignedNames = filter (`notElem` localDeclNames) assignedNames
    checkedNames <- mapM (checkAssignedName stageTable) newAssignedNames

    -- Step 4: Generate pipeline register declarations (name_stageName = 0)
    -- for each module-scope variable assigned in this stage.
    let newDecls = map (buildRegisterDecl stageTable) checkedNames

    -- Step 5: Rename local declarations to name_stageName and substitute
    -- all references to local names in statement and always-block items.
    let renamedDeclItems   = map (renameLocalDecl localRenaming) declItems
    let renamedStmtItems   = map (substituteInModuleItem localRenaming) stmtItems
    let renamedAlwaysItems = map (substituteInModuleItem localRenaming) alwaysItems

    -- Step 6: Reassemble: renamed decls, new pipeline registers, renamed
    -- statements, then renamed explicit always blocks.
    return (renamedDeclItems ++ newDecls ++ renamedStmtItems ++ renamedAlwaysItems, stageTable)

  where

    declItems   = [item | item@(MIPackageItem (Decl _)) <- items]
    stmtItems   = [item | item@(Statement _)            <- items]
    alwaysItems = [item | item@(AlwaysC _ _)            <- items]
    stmts       = [stmt | Statement stmt                <- items]

    addDecl (table, names) (MIPackageItem (Decl (Variable _ declType name _ initializer))) = do
        checkDuplicate table name
        return (Map.insert name (declType, StageScope stageName, initializer) table, names ++ [name])
    addDecl (table, names) (MIPackageItem (Decl (Net _ _ _ declType name _ initializer))) = do
        checkDuplicate table name
        return (Map.insert name (declType, StageScope stageName, initializer) table, names ++ [name])
    addDecl accumulator _ = return accumulator

    checkDuplicate table name =
        case Map.lookup name table of
            Just (_, ModuleScope, _) -> do
                hPutStrLn stderr $ "Error in GenerateVariables.processStage: "
                    ++ "variable '" ++ name ++ "' already declared at module scope"
                exitFailure
            Just (_, StageScope existingStageName, _) -> do
                hPutStrLn stderr $ "Error in GenerateVariables.processStage: "
                    ++ "variable '" ++ name ++ "' already declared in stage '"
                    ++ existingStageName ++ "'"
                exitFailure
            Nothing -> return ()

    checkAssignedName table name =
        case Map.lookup name table of
            Nothing -> do
                hPutStrLn stderr $ "Error in GenerateVariables.processStage: "
                    ++ "assignment to undeclared variable '" ++ name
                    ++ "' in stage '" ++ stageName ++ "'"
                exitFailure
            Just _ -> return name

    buildRegisterDecl table name =
        case Map.lookup name table of
            Just (declType, _, initializer) ->
                MIPackageItem (Decl (Variable Local declType
                    (name ++ "_" ++ stageName) [] initializer))
            Nothing ->
                error "GenerateVariables.buildRegisterDecl: name not in table"

-- -------------------------------------------------------
-- Top-level entry point
-- -------------------------------------------------------

-- | Processes all stages in a module's item list, threading the symbol
-- table through in order so each stage sees declarations from all
-- previous stages.
generateVariables :: [ModuleItem] -> IO [ModuleItem]
generateVariables items = do

    -- Initialize symbol table from module-scope declarations.
    let symbolTable = initSymbolTable items

    -- Fold over items, threading the symbol table through each stage.
    (_, resultItems) <- foldM processItem (symbolTable, []) items
    return (reverse resultItems)

  where

    processItem (table, accumulated) (StageC kw (Stage name stageItems)) = do
        (expanded, table') <- processStage table name stageItems
        return (table', StageC kw (Stage name expanded) : accumulated)

    processItem (table, accumulated) item =
        return (table, item : accumulated)
