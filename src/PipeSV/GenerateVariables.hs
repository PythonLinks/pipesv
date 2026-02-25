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

type SymbolTable = Map.Map Identifier (Type, Location)

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
-- Symbol table initialization
-- -------------------------------------------------------

-- | Builds the initial symbol table from module-scope declarations.
-- Stage items are ignored — they are added later by processStage.
initSymbolTable :: [ModuleItem] -> SymbolTable
initSymbolTable items = Map.fromList $ concatMap extractDecl items
  where

    extractDecl (MIPackageItem (Decl (Variable _ declType name _ _))) =
        [(name, (declType, ModuleScope))]

    extractDecl (MIPackageItem (Decl (Net _ _ _ declType name _ _))) =
        [(name, (declType, ModuleScope))]

    extractDecl _ = []

-- -------------------------------------------------------
-- Stage processing
-- -------------------------------------------------------

-- | Processes a single stage: adds stage-local declarations to the symbol
-- table, collects all assigned variable names, and inserts pipeline register
-- declarations (name_stageName) into the stage item list.
processStage :: SymbolTable -> String -> [ModuleItem] -> IO ([ModuleItem], SymbolTable)
processStage symbolTable stageName items = do

    -- Step 1: Add stage-local declarations to the symbol table,
    -- checking for duplicates, and collect their names.
    (stageTable, localDeclNames) <- foldM addDecl (symbolTable, []) declItems

    -- Step 2: Collect LHS root names from all assignment statements.
    let assignedNames = nub $ concatMap stmtAssignedNames stmts

    -- Step 3: Look up each assigned name not already covered by an explicit
    -- declaration; error if not found in the symbol table.
    let newAssignedNames = filter (`notElem` localDeclNames) assignedNames
    checkedNames <- mapM (checkAssignedName stageTable) newAssignedNames

    -- Step 4: Build pipeline register declarations for all collected names.
    let collectedNames = nub (localDeclNames ++ checkedNames)
    let newDecls       = map (buildRegisterDecl stageTable) collectedNames

    -- Step 5: Reassemble: original decls, new registers, then statements.
    return (declItems ++ newDecls ++ stmtItems, stageTable)

  where

    declItems = [item | item@(MIPackageItem (Decl _)) <- items]
    stmtItems = [item | item@(Statement _)            <- items]
    stmts     = [stmt | Statement stmt                <- items]

    addDecl (table, names) (MIPackageItem (Decl (Variable _ declType name _ _))) = do
        checkDuplicate table name
        return (Map.insert name (declType, StageScope stageName) table, names ++ [name])
    addDecl (table, names) (MIPackageItem (Decl (Net _ _ _ declType name _ _))) = do
        checkDuplicate table name
        return (Map.insert name (declType, StageScope stageName) table, names ++ [name])
    addDecl accumulator _ = return accumulator

    checkDuplicate table name =
        case Map.lookup name table of
            Just (_, ModuleScope) -> do
                hPutStrLn stderr $ "Error in GenerateVariables.processStage: "
                    ++ "variable '" ++ name ++ "' already declared at module scope"
                exitFailure
            Just (_, StageScope existingStageName) -> do
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
            Just (declType, _) ->
                MIPackageItem (Decl (Variable Local declType
                    (name ++ "_" ++ stageName) [] Nil))
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
