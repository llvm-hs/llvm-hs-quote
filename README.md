llvm-hs-quote
=============

[![Build Status](https://travis-ci.org/llvm-hs/llvm-hs-quote.svg?branch=master)](https://travis-ci.org/llvm-hs/llvm-hs-quote)

`llvm-hs-quote` is a quasiquoting-library for llvm-hs.  It aims to support
all language constructs of LLVM.

`llvm-hs-quote` provides both quasiquotes and antiquotes. The following trivial
example uses both a quasiquote and an antiquote.

```haskell
alloc :: Type -> Instruction
alloc t = [lli|alloc $type:t|]
```

`LLVM.Quote.LLVM` provides quasiquoters or antiquoters for the following types.
For each type `a`, there's also a corresponding quasiquoter or antiquoter for
`CodeGen a` with an `M` added to the end of the name. For example,
`Definition`'s quasiquoter is `lldef`; the corresponding quasiquoter for
`CodeGen Definition` is `lldefM`. Its antiquoter is `$def:`; the corresponding
antiquoter for `CodeGen Definition` is `$defM:`.

AST Type                             | Quasiquoter | Antiquoter
-------------------------------------| ----------- | ----------
`LLVM.AST.Module`                    | `llmod`     |
`LLVM.AST.Definition`                | `lldef`     | `$def:`
`[LLVM.AST.Definition]`              |             | `$defs:`
`LLVM.AST.Global`                    | `llg`       |
`LLVM.AST.Instruction.Instruction`   | `lli`       | `$instr:`
`[LLVM.AST.Instruction.Instruction]` |             | `$instrs:`
`LLVM.AST.DataLayout.DataLayout`     |             | `$dl:`
`LLVM.Quote.AST.TargetTriple`        |             | `$tt:`
`LLVM.AST.BasicBlock`                |             | `$bb:`
`[LLVM.AST.BasicBlock]`              |             | `$bbs:`
`LLVM.AST.Type.Type`                 |             | `$type:`
`LLVM.AST.Operand.Operand`           |             | `$opr:`
`LLVM.AST.Constant.Constant`         |             | `$const:`
`LLVM.AST.Name.Name` (local)         |             | `$id:`
`LLVM.AST.Name.Name` (global)        |             | `$gid:`
`LLVM.AST.Parameter`                 |             | `$param:`
`[LLVM.AST.Parameter]`               |             | `$params:`


License
-------

Copyright (c) 2014, Timo von Holtz
