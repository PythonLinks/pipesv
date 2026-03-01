module PipeSV.NameResolution where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

import Language.SystemVerilog.AST
import PipeSV.StageContext

-- | Converts an Offset to a signed Int. Negative offsets refer to past stages.
-- Offset Negative 1  →  -1
-- Offset Positive 1  →   1
offsetToInt :: Offset -> Int
offsetToInt (Offset Positive n) =   fromIntegral n
offsetToInt (Offset Negative n) = -(fromIntegral n)

-- | Edits a stage expression to a plain Verilog expression by renaming
-- the identifier to its target-stage name via editName.
editStageExpr :: StageContext -> Int -> StageExpression -> IO Expr

-- a#{-1}      →  a_previousStageName
--             →  Ident "a_previousStageName"
editStageExpr context currentStageIndex (StageOffset ident stageIdentifier) = do
    newName <- editName context currentStageIndex ident stageIdentifier
    return (Ident newName)

-- a#{-1}[3]   →  a_previousStageName[3]
--             →  Bit (Ident "a_previousStageName") 3
editStageExpr context currentStageIndex (StageSelect ident stageIdentifier index) = do
    newName <- editName context currentStageIndex ident stageIdentifier
    return (Bit (Ident newName) index)

-- a#{-1}[3:0] →  a_previousStageName[3:0]
--             →  Range (Ident "a_previousStageName") mode r
editStageExpr context currentStageIndex (StageRange ident stageIdentifier (mode, r)) = do
    newName <- editName context currentStageIndex ident stageIdentifier
    return (Range (Ident newName) mode r)

-- | Calculates the new name of a reg or wire on the right-hand side.
-- Dispatches to editNameByIndex after resolving the StageIdentifier to a target index.
editName :: StageContext -> Int -> String -> StageIdentifier -> IO String
editName context currentStageIndex ident (StageByOffset offset) =
    editNameByIndex context currentStageIndex ident
        (currentStageIndex + offsetToInt offset)
editName context _ ident (StageByName name) =
    case Map.lookup name (nameToIndex context) of
        Nothing -> do
            hPutStrLn stderr $ "Error in file " ++ fileName context
                ++ " in the function NameResolution.editName: unknown stage name '"
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
                hPutStrLn stderr $ "Error in file " ++ fileName context
                    ++ " in the function NameResolution.editNameByIndex: '"
                    ++ ident ++ "' is not assigned in stage '"
                    ++ (contextStageNames context !! targetIndex)
                    ++ "', so '" ++ ident ++ "_"
                    ++ (contextStageNames context !! targetIndex) ++ "' does not exist"
                exitFailure
            else do
                hPutStrLn stderr $ "Warning in file " ++ fileName context
                    ++ " in the function NameResolution.editNameByIndex: '"
                    ++ ident ++ "' same-stage reference is redundant, use '"
                    ++ ident ++ "' directly"
                return ident

    -- Target stage is before the first stage: error
    | targetIndex < -1 = do
        hPutStrLn stderr $ "Error in file " ++ fileName context
            ++ " in the function NameResolution.editNameByIndex: '"
            ++ ident ++ "#{" ++ show (targetIndex - currentStageIndex) ++ "}'"
            ++ " in stage '" ++ (contextStageNames context !! currentStageIndex)
            ++ "' refers to a point before the start of the pipeline"
        exitFailure

    -- Target stage is after the last stage: error
    | targetIndex > lastIndex = do
        hPutStrLn stderr $ "Error in file " ++ fileName context
            ++ " in the function NameResolution.editNameByIndex: '"
            ++ ident ++ "#{" ++ show (targetIndex - currentStageIndex) ++ "}'"
            ++ " in stage '" ++ (contextStageNames context !! currentStageIndex)
            ++ "' refers to a point after the end of the pipeline"
        exitFailure

    -- Target is -1: this is a port input, leave the name unchanged
    | targetIndex == -1 =
        return ident

    -- Target is a valid stage: rename to ident_stageName
    | otherwise = do
        let newName = ident ++ "_" ++ (contextStageNames context !! targetIndex)
        if Set.member newName (validRHSNames context)
            then return newName
            else do
                hPutStrLn stderr $ "Error in file " ++ fileName context
                    ++ " in the function NameResolution.editNameByIndex: '"
                    ++ newName ++ "' does not exist; '"
                    ++ ident ++ "' is not assigned in stage '"
                    ++ (contextStageNames context !! targetIndex) ++ "'"
                exitFailure

  where
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
        hPutStrLn stderr $ "Error in file " ++ fileName context
            ++ " in the function NameResolution.applyDefaultOffset: "
            ++ "default -1 offset out of bounds for '" ++ name ++ "'"
        exitFailure
    | otherwise = do
        let newName = name ++ "_" ++ (contextStageNames context !! tgt)
        if Set.member newName (validRHSNames context)
            then return (Ident newName)
            else do
                hPutStrLn stderr $ "Error in file " ++ fileName context
                    ++ " in the function NameResolution.applyDefaultOffset: '"
                    ++ newName ++ "' does not exist; '"
                    ++ name ++ "' is not assigned in stage '"
                    ++ (contextStageNames context !! tgt) ++ "'"
                exitFailure
  where
    tgt = currentStageIndex - 1

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
    renameRoot stageName (LHSBit   subLhs e)     = do subLhs' <- renameRoot stageName subLhs; return (LHSBit subLhs' e)
    renameRoot stageName (LHSRange subLhs m r)   = do subLhs' <- renameRoot stageName subLhs; return (LHSRange subLhs' m r)
    renameRoot stageName (LHSDot   subLhs x)     = do subLhs' <- renameRoot stageName subLhs; return (LHSDot subLhs' x)
    renameRoot stageName (LHSConcat lhss)     = do lhss' <- mapM (renameRoot stageName) lhss; return (LHSConcat lhss')
    renameRoot stageName (LHSStream o e lhss) = do lhss' <- mapM (renameRoot stageName) lhss; return (LHSStream o e lhss')
