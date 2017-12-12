llvm-hs-quote
=============

**Initial fork of
[`llvm-general-quote`](https://github.com/tvh/llvm-general-quote/issues/6) to
support the modern `llvm-hs` family of libraries.**

`llvm-hs-quote` is a quasiquoting-library for llvm-general.  It aims to support
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

AST Type                                     | Quasiquoter | Antiquoter
-------------------------------------------- | ----------- | ----------
`LLVM.General.AST.Module`                    | `llmod`     |
`LLVM.General.AST.Definition`                | `lldef`     | `$def:`
`[LLVM.General.AST.Definition]`              |             | `$defs:`
`LLVM.General.AST.Global`                    | `llg`       |
`LLVM.General.AST.Instruction.Instruction`   | `lli`       | `$instr:`
`[LLVM.General.AST.Instruction.Instruction]` |             | `$instrs:`
`LLVM.General.AST.DataLayout.DataLayout`     |             | `$dl:`
`LLVM.General.Quote.AST.TargetTriple`        |             | `$tt:`
`LLVM.General.AST.BasicBlock`                |             | `$bb:`
`[LLVM.General.AST.BasicBlock]`              |             | `$bbs:`
`LLVM.General.AST.Type.Type`                 |             | `$type:`
`LLVM.General.AST.Operand.Operand`           |             | `$opr:`
`LLVM.General.AST.Constant.Constant`         |             | `$const:`
`LLVM.General.AST.Name.Name` (local)         |             | `$id:`
`LLVM.General.AST.Name.Name` (global)        |             | `$gid:`
`LLVM.General.AST.Parameter`                 |             | `$param:`
`[LLVM.General.AST.Parameter]`               |             | `$params:`

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
