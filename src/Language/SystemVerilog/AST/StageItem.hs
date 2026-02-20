


module Language.SystemVerilog.AST.StageItem
    ( StageItem    (..))
    where


import Language.SystemVerilog.AST.ShowHelp()
import Language.SystemVerilog.AST.GenItem (GenItem)
import Language.SystemVerilog.AST.ModuleItem (AlwaysKW,
          AssignOption,PortBinding)
import Language.SystemVerilog.AST.LHS (LHS)
import Language.SystemVerilog.AST.Stmt (Stmt)
import Language.SystemVerilog.AST.Type (Identifier)
import Language.SystemVerilog.AST.Expr (ParamBinding, Range, Expr)



data StageItem
     = SIAlwaysC    AlwaysKW Stmt
    | SIAssign     AssignOption LHS Expr
    | SIGenvar     Identifier
    | SIGenerate   [GenItem]
    | SIInstance   Identifier [ParamBinding] Identifier [Range] [PortBinding]
    deriving Eq