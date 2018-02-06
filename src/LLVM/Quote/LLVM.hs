module LLVM.Quote.LLVM (
    llmod,
    lldef,
    llg,
    -- llbb,
    -- llbbs,
    lli,
    llmodM,
    lldefM,
    llgM,
    -- llbbM,
    -- llbbsM,
    lliM
) where

import LLVM.Quote.Base (CodeGen, quasiquote, quasiquoteM, TQuasiQuoter(unTQuasiQuoter))
import qualified LLVM.Quote.AST as A
import qualified LLVM.Quote.Parser.Parser as P
import qualified LLVM.AST as L

import Language.Haskell.TH.Quote

exts :: [A.Extensions]
exts = []

-- | Quasiquoter for 'LLVM.AST.Module'
llmod :: QuasiQuoter
llmod = unTQuasiQuoter
          (quasiquote exts P.parseModule :: TQuasiQuoter L.Module)

-- | Quasiquoter for 'LLVM.AST.Definition'
lldef :: QuasiQuoter
lldef = unTQuasiQuoter
          (quasiquote exts P.parseDefinition :: TQuasiQuoter L.Definition)

-- | Quasiquoter for 'LLVM.AST.Global'
llg :: QuasiQuoter
llg = unTQuasiQuoter (quasiquote exts P.parseGlobal :: TQuasiQuoter L.Global)

-- | Quasiquoter for 'LLVM.AST.Instruction.Instruction'
lli :: QuasiQuoter
lli = unTQuasiQuoter
        (quasiquote exts P.parseInstruction :: TQuasiQuoter L.Instruction)


-- | Quasiquoter for 'LLVM.AST.Module'
llmodM :: QuasiQuoter
llmodM = unTQuasiQuoter
          (quasiquoteM exts P.parseModule :: TQuasiQuoter (CodeGen L.Module))

-- | Quasiquoter for 'LLVM.AST.Definition'
lldefM :: QuasiQuoter
lldefM = unTQuasiQuoter
          (quasiquoteM exts P.parseDefinition :: TQuasiQuoter (CodeGen L.Definition))

-- | Quasiquoter for 'LLVM.AST.Global'
llgM :: QuasiQuoter
llgM = unTQuasiQuoter (quasiquoteM exts P.parseGlobal :: TQuasiQuoter (CodeGen L.Global))

-- | Quasiquoter for 'LLVM.AST.Instruction.Instruction'
lliM :: QuasiQuoter
lliM = unTQuasiQuoter
        (quasiquoteM exts P.parseInstruction :: TQuasiQuoter (CodeGen L.Instruction))
