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

In addition to this, it supports using mutable variables and control structures instead of pure SSA form.
This is translated automatically into SSA through appropriate renaming.

Example:

```haskell
[lldef|
  define i64 @foo(i64 %start, i64 %end) {
    entry:
      %x = i64 0

    for:
      for i64 %i in %start to %end {
          %x = add i64 %i, %x
      }

    exit:
      ret i64 %x
  }
  |]
```

this would be transformed into:

```haskell
define i64 @foo(i64 %start, i64 %end) {
entry:
  br label %for.head

for.head:                      ; preds = %n0, %entry
  %x.12 = phi i64 [ 0, %entry ], [ %x.6, %n0 ]
  %i.4 = phi i64 [ %start, %entry ], [ %i.9, %n0 ]
  %for.cond.3 = icmp slt i64 %i.4, %end
  br i1 %for.cond.3, label %n0, label %for.end

n0:                            ; preds = %for.head
  %x.6 = add i64 %i.4, %x.12
  %i.9 = add nuw nsw i64 %i.4, 1
  br label %for.head

for.end:                       ; preds = %for.head
  ret i64 %x.12
}
```

License
-------

Copyright (c) 2014, Timo von Holtz
