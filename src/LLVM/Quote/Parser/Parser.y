{
module LLVM.Quote.Parser.Parser where

import Control.Monad (forM_,
                      when,
                      unless,
                      liftM)
import Control.Monad.Except
import Data.List (intersperse)
import Data.List.Split
import Data.Loc
import Data.String (fromString)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, catMaybes, listToMaybe)
import Data.Word
import Data.Char (ord)
import Data.ByteString.Short (ShortByteString)
import Text.PrettyPrint.Mainland

import qualified Data.ByteString.Char8 as BS


import LLVM.Quote.Parser.Lexer
import LLVM.Quote.Parser.Monad
import qualified LLVM.Quote.Parser.Tokens as T
import qualified LLVM.Quote.AST as A
import qualified LLVM.AST.Float as A
import qualified LLVM.AST.Type as LA
import qualified LLVM.AST.Linkage as A
import qualified LLVM.AST.Visibility as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.AddrSpace as A
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.InlineAssembly as A ( Dialect(..) )
import qualified LLVM.AST.Instruction as A ( Atomicity(..), MemoryOrdering(..), SynchronizationScope(..), TailCallKind(..) )
import qualified LLVM.AST.IntegerPredicate as AI
import qualified LLVM.AST.FloatingPointPredicate as AF
import qualified LLVM.AST.RMWOperation as AR
import qualified LLVM.AST.DataLayout as A
  ( Endianness(..), AlignmentInfo(..), AlignType(..) )
import qualified LLVM.DataLayout as A
}

%token
 INT                 { L _ (T.TintConst $$) }
 FLOAT               { L _ (T.TfloatConst $$) }
 STRING              { L _ (T.TstringConst $$) }
 CSTRING             { L _ (T.TcstringConst $$) }
 NAMED_GLOBAL        { L _ (T.Tnamed T.Global $$) }
 NAMED_LOCAL         { L _ (T.Tnamed T.Local $$) }
 NAMED_META          { L _ (T.Tnamed T.Meta $$) }
 UNNAMED_GLOBAL      { L _ (T.Tunnamed T.Global $$) }
 UNNAMED_LOCAL       { L _ (T.Tunnamed T.Local $$) }
 UNNAMED_META        { L _ (T.Tunnamed T.Meta $$) }
 JUMPLABEL           { L _ (T.TjumpLabel $$) }
 '('    { L _ T.Tlparen }
 ')'    { L _ T.Trparen }
 '['    { L _ T.Tlbrack }
 ']'    { L _ T.Trbrack }
 '{'    { L _ T.Tlbrace }
 '}'    { L _ T.Trbrace }
 '<'    { L _ T.Tlt }
 '>'    { L _ T.Tgt }
 ','    { L _ T.Tcomma }
 '='    { L _ T.Tassign }
 '*'    { L _ T.Tstar }
 '-'    { L _ T.Tminus }
 '!'    { L _ T.Tbang }
 '...'  { L _ T.Tpoints }
 'x'    { L _ T.Tx }
 'zeroinitializer'  { L _ T.Tzeroinitializer }
 'undef'            { L _ T.Tundef }
 'ret'              { L _ T.Tret }
 'br'               { L _ T.Tbr }
 'switch'           { L _ T.Tswitch }
 'indirectbr'       { L _ T.Tindirectbr }
 'invoke'           { L _ T.Tinvoke }
 'resume'           { L _ T.Tresume }
 'unreachable'      { L _ T.Tunreachable }
 'add'              { L _ T.Tadd }
 'fadd'             { L _ T.Tfadd }
 'sub'              { L _ T.Tsub }
 'fsub'             { L _ T.Tfsub }
 'mul'              { L _ T.Tmul }
 'fmul'             { L _ T.Tfmul }
 'udiv'             { L _ T.Tudiv }
 'sdiv'             { L _ T.Tsdiv }
 'fdiv'             { L _ T.Tfdiv }
 'urem'             { L _ T.Turem }
 'srem'             { L _ T.Tsrem }
 'frem'             { L _ T.Tfrem }
 'shl'              { L _ T.Tshl }
 'lshr'             { L _ T.Tlshr }
 'ashr'             { L _ T.Tashr }
 'and'              { L _ T.Tand }
 'or'               { L _ T.Tor }
 'xor'              { L _ T.Txor }
 'alloca'           { L _ T.Talloca }
 'load'             { L _ T.Tload }
 'store'            { L _ T.Tstore }
 'getelementptr'    { L _ T.Tgetelementptr }
 'fence'            { L _ T.Tfence }
 'cmpxchg'          { L _ T.Tcmpxchg }
 'atomicrmw'        { L _ T.Tatomicrmw }
 'trunc'            { L _ T.Ttrunc }
 'zext'             { L _ T.Tzext }
 'sext'             { L _ T.Tsext }
 'fptoui'           { L _ T.Tfptoui }
 'fptosi'           { L _ T.Tfptosi }
 'uitofp'           { L _ T.Tuitofp }
 'sitofp'           { L _ T.Tsitofp }
 'fptrunc'          { L _ T.Tfptrunc }
 'fpext'            { L _ T.Tfpext }
 'ptrtoint'         { L _ T.Tptrtoint }
 'inttoptr'         { L _ T.Tinttoptr }
 'bitcast'          { L _ T.Tbitcast }
 'addrspacecast'    { L _ T.Taddrspacecast }
 'icmp'             { L _ T.Ticmp }
 'fcmp'             { L _ T.Tfcmp }
 'phi'              { L _ T.Tphi }
 'call'             { L _ T.Tcall }
 'select'           { L _ T.Tselect }
 'va_arg'           { L _ T.Tvaarg }
 'extractelement'   { L _ T.Textractelement }
 'insertelement'    { L _ T.Tinsertelement }
 'shufflevector'    { L _ T.Tshufflevector }
 'extractvalue'     { L _ T.Textractvalue }
 'insertvalue'      { L _ T.Tinsertvalue }
 'landingpad'       { L _ T.Tlandingpad }
 'eq'               { L _ T.Teq }
 'ne'               { L _ T.Tne }
 'ugt'              { L _ T.Tugt }
 'uge'              { L _ T.Tuge }
 'ult'              { L _ T.Tult }
 'ule'              { L _ T.Tule }
 'sgt'              { L _ T.Tsgt }
 'sge'              { L _ T.Tsge }
 'slt'              { L _ T.Tslt }
 'sle'              { L _ T.Tsle }
 'false'            { L _ T.Tfalse }
 'oeq'              { L _ T.Toeq }
 'ogt'              { L _ T.Togt }
 'oge'              { L _ T.Toge }
 'olt'              { L _ T.Tolt }
 'ole'              { L _ T.Tole }
 'one'              { L _ T.Tone }
 'ord'              { L _ T.Tord }
 'uno'              { L _ T.Tuno }
 'ueq'              { L _ T.Tueq }
 'une'              { L _ T.Tune }
 'true'             { L _ T.Ttrue }
 'label'            { L _ T.Tlabel }
 'volatile'         { L _ T.Tvolatile }
 'inbounds'         { L _ T.Tinbounds }
 'align'            { L _ T.Talign }
 'nnan'             { L _ T.Tnnan }
 'ninf'             { L _ T.Tninf }
 'nsz'              { L _ T.Tnsz }
 'arcp'             { L _ T.Tarcp }
 'fast'             { L _ T.Tfast }
 'to'               { L _ T.Tto }
 'downto'           { L _ T.Tdownto }
 'nsw'              { L _ T.Tnsw }
 'nuw'              { L _ T.Tnuw }
 'target'           { L _ T.Ttarget }
 'datalayout'       { L _ T.Tdatalayout }
 'triple'           { L _ T.Ttriple }
 'define'           { L _ T.Tdefine }
 'declare'          { L _ T.Tdeclare }
 'void'             { L _ T.Tvoid }
 'half'             { L _ T.Thalf }
 'float'            { L _ T.Tfloat }
 'double'           { L _ T.Tdouble }
 INTEGERTYPE        { L _ (T.TintegerType $$) }
 'metadata'         { L _ T.Tmetadata }
 'zeroext'          { L _ T.Tzeroext }
 'signext'          { L _ T.Tsignext }
 'inreg'            { L _ T.Tinreg }
 'byval'            { L _ T.Tbyval }
 'sret'             { L _ T.Tsret }
 'noalias'          { L _ T.Tnoalias }
 'nocapture'        { L _ T.Tnocapture }
 'nest'             { L _ T.Tnest }
 'alignstack'       { L _ T.Talignstack }
 'alwaysinline'     { L _ T.Talwaysinline }
 'inlinehint'       { L _ T.Tinlinehint }
 'naked'            { L _ T.Tnaked }
 'noimplicitfloat'  { L _ T.Tnoimplicitfloat }
 'noinline'         { L _ T.Tnoinline }
 'nonlazybind'      { L _ T.Tnonlazybind }
 'noredzone'        { L _ T.Tnoredzone }
 'noreturn'         { L _ T.Tnoreturn }
 'nounwind'         { L _ T.Tnounwind }
 'optsize'          { L _ T.Toptsize }
 'readnone'         { L _ T.Treadnone }
 'readonly'         { L _ T.Treadonly }
 'ssp'              { L _ T.Tssp }
 'sspreq'           { L _ T.Tsspreq }
 'uwtable'          { L _ T.Tuwtable }
 'global'           { L _ T.Tglobal }
 'constant'         { L _ T.Tconstant }
 'alias'            { L _ T.Talias }
 'unwind'           { L _ T.Tunwind }
 'unordered'        { L _ T.Tunordered }
 'monotonic'        { L _ T.Tmonotonic }
 'acquire'          { L _ T.Tacquire }
 'release'          { L _ T.Trelease }
 'acq_rel'          { L _ T.Tacq_rel }
 'seq_cst'          { L _ T.Tseq_cst }
 'singlethread'     { L _ T.Tsinglethread }
 'xchg'             { L _ T.Txchg }
 'nand'             { L _ T.Tnand }
 'max'              { L _ T.Tmax }
 'min'              { L _ T.Tmin }
 'umax'             { L _ T.Tumax }
 'umin'             { L _ T.Tumin }
 'cleanup'          { L _ T.Tcleanup }
 'catch'            { L _ T.Tcatch }
 'filter'           { L _ T.Tfilter }
 'personality'      { L _ T.Tpersonality }
 'private'          { L _ T.Tprivate }
 'internal'         { L _ T.Tinternal }
 'available_externally'
                    { L _ T.Tavailable_externally }
 'linkonce'         { L _ T.Tlinkonce }
 'weak'             { L _ T.Tweak }
 'common'           { L _ T.Tcommon }
 'appending'        { L _ T.Tappending }
 'extern_weak'      { L _ T.Textern_weak }
 'linkonce_odr'     { L _ T.Tlinkonce_odr }
 'weak_odr'         { L _ T.Tweak_odr }
 'external'         { L _ T.Texternal }
 'default'          { L _ T.Tdefault }
 'hidden'           { L _ T.Thidden }
 'protected'        { L _ T.Tprotected }
 'ccc'              { L _ T.Tccc }
 'fastcc'           { L _ T.Tfastcc }
 'coldcc'           { L _ T.Tcoldcc }
 'cc'               { L _ T.Tcc }
 'atomic'           { L _ T.Tatomic }
 'null'             { L _ T.Tnull }
 'exact'            { L _ T.Texact }
 'addrspace'        { L _ T.Taddrspace }
 'blockaddress'     { L _ T.Tblockaddress }
 'module'           { L _ T.Tmodule }
 'asm'              { L _ T.Tasm }
 'type'             { L _ T.Ttype }
 'opaque'           { L _ T.Topaque }
 'sideeffect'       { L _ T.Tsideeffect }
 'inteldialect'     { L _ T.Tinteldialect }
 'section'          { L _ T.Tsection }
 'gc'               { L _ T.Tgc }
 'tail'             { L _ T.Ttail }
 'musttail'         { L _ T.Tmusttail }
 'notail'           { L _ T.Tnotail }

 'for'              { L _ T.Tfor }
 'in'               { L _ T.Tin }
 'step'             { L _ T.Tstep }
 'if'               { L _ T.Tif }
 'else'             { L _ T.Telse }
 'while'            { L _ T.Twhile }

 ANTI_DL            { L _ (T.Tanti_dl $$) }
 ANTI_TT            { L _ (T.Tanti_tt $$) }
 ANTI_DEF           { L _ (T.Tanti_def $$) }
 ANTI_DEFS          { L _ (T.Tanti_defs $$) }
 ANTI_BB            { L _ (T.Tanti_bb $$) }
 ANTI_BBS           { L _ (T.Tanti_bbs $$) }
 ANTI_INSTR         { L _ (T.Tanti_instr $$) }
 ANTI_INSTRS        { L _ (T.Tanti_instrs $$) }
 ANTI_TYPE          { L _ (T.Tanti_type $$) }
 ANTI_OPR           { L _ (T.Tanti_opr $$) }
 ANTI_CONST         { L _ (T.Tanti_const $$) }
 ANTI_ID            { L _ (T.Tanti_id $$) }
 ANTI_GID           { L _ (T.Tanti_gid $$) }
 ANTI_PARAM         { L _ (T.Tanti_param $$) }
 ANTI_PARAMS        { L _ (T.Tanti_params $$) }

%monad { P } { >>= } { return }
%lexer { lexer } { L _ T.Teof }
%tokentype { (L T.Token) }
%error { happyError }

%name parseModule       module
%name parseDefinition   definition
%name parseGlobal       global
%name parseInstruction  instruction

%%

{------------------------------------------------------------------------------
 -
 - Constants
 -
 -----------------------------------------------------------------------------}

{- Constants that require a type (distinct from cConstant) -}
fConstant :: { A.Type -> A.Constant }
fConstant :
    INT                   { intConstant $1 }
  | '-' INT               { intConstant (-$2) }
  | 'true'                { intConstant 1 }
  | 'false'               { intConstant 0 }
  | FLOAT                 { floatConstant $1 }
  | '-' FLOAT             { floatConstant (-$2) }
  | 'zeroinitializer'     { A.Null }
  | 'null'                { A.Null }
  | '{' constantList '}'  { \_ -> A.Struct Nothing False (rev $2) }
  | '[' constantList ']'  { \t -> A.Array (A.elementType t) (rev $2) }
  | '<' constantList '>'  { \_ -> A.Vector (rev $2) }
  | 'blockaddress' '(' globalName ',' name ')'
                          { \_ -> A.BlockAddress $3 $5 }
  | 'undef'               { A.Undef }
  | globalName            { \t -> A.GlobalReference t $1 }
  
{- Constants that don't require a type -}
cConstant :: { A.Constant }
cConstant :
    ANTI_CONST            { A.AntiConstant (fromString $1) }
  | CSTRING               { A.Array (A.IntegerType 8) (map (A.Int 8 . fromIntegral . ord) $1) }
    
constant :: { A.Type -> A.Constant }
constant :
    fConstant             { $1 }  
  | cConstant             { \_ -> $1 }

tConstant :: { A.Constant }
tConstant :
    type constant         { $2 $1 }
  | cConstant             { $1 }

mConstant :: { A.Type -> Maybe A.Constant }
mConstant :
    {- empty -}            { \_ -> Nothing }
  | constant               { Just . $1 }
  
constantList :: { RevList A.Constant }
constantList :
    tConstant                    { RCons $1 RNil }
  | constantList ',' tConstant   { RCons $3 $1 }
    
{------------------------------------------------------------------------------
 -
 - Operands
 -
 -----------------------------------------------------------------------------}

operand :: { A.Type -> A.Operand }
operand :
    fConstant           { A.ConstantOperand . $1 }
  | name                { \t -> A.LocalReference t $1 }
  | metadata            { \A.MetadataType -> A.MetadataOperand $1 }
  | cOperand            { \_ -> $1 }

mOperand :: { Maybe A.Operand }
mOperand :
    {- empty -}      { Nothing }
  | ',' tOperand     { Just $2 }

tOperand :: { A.Operand }
tOperand :
    type operand        { $2 $1 }
  | cOperand            { $1 }
    
{- Operands that don't require a type -}
cOperand :: { A.Operand }
cOperand :
    ANTI_OPR            { A.AntiOperand (fromString $1) }
  | cConstant           { A.ConstantOperand $1 }

{- Binary operator operands -}
binOperands :: { (A.Operand, A.Operand) }
binOperands :
    type operand ',' operand    { ($2 $1, $4 $1) }
  | cOperand ',' cOperand       { ($1, $3) }

{------------------------------------------------------------------------------
 -
 - Instructions
 -
 -----------------------------------------------------------------------------}

nuw :: { Bool }
nuw :
    {- empty  -}        { False }
  | 'nuw'               { True }

nsw :: { Bool }
nsw :
    {- empty  -}        { False }
  | 'nsw'               { True }

nnan :: { Bool }
nnan :
    {- empty -}    { False }
  | 'nnan'         { True }

ninf :: { Bool }
ninf :
    {- empty -}    { False }
  | 'ninf'         { True }

nsz :: { Bool }
nsz :
    {- empty -}    { False }
  | 'nsz'          { True }

arcp :: { Bool }
arcp :
    {- empty -}    { False }
  | 'arcp'         { True }

fmflags :: { A.FastMathFlags }
fmflags :
    nnan ninf nsz arcp  { if not (or [$1, $2, $3, $4])
                            then A.NoFastMathFlags
			    else A.FastMathFlags $1 $2 $3 $4 }
  | 'fast'              { A.UnsafeAlgebra }

volatile :: { Bool }
volatile :
    {- empty  -}        { False }
  | 'volatile'          { True }

alignment :: { Word32 }
alignment :
    {- empty -}      { 0 }
  | ',' 'align' INT  { fromIntegral $3 }

inBounds :: { Bool }
inBounds :
    {- empty -}         { False }
  | 'inbounds'          { True }

indices :: { RevList A.Operand }
indices :
    {- empty -}            { RNil }
  | indices ',' tOperand   { RCons $3 $1 }

intP :: { AI.IntegerPredicate }
intP :
   'eq'              { AI.EQ }
 | 'ne'              { AI.NE }
 | 'ugt'             { AI.UGT }
 | 'uge'             { AI.UGE }
 | 'ult'             { AI.ULT }
 | 'ule'             { AI.ULE }
 | 'sgt'             { AI.SGT }
 | 'sge'             { AI.SGE }
 | 'slt'             { AI.SLT }
 | 'sle'             { AI.SLE }

fpP :: { AF.FloatingPointPredicate }
fpP :
      'false'          { AF.False }
    | 'oeq'            { AF.OEQ }
    | 'ogt'            { AF.OGT }
    | 'oge'            { AF.OGE }
    | 'olt'            { AF.OLT }
    | 'ole'            { AF.OLE }
    | 'one'            { AF.ONE }
    | 'ord'            { AF.ORD }
    | 'uno'            { AF.UNO }
    | 'ueq'            { AF.UEQ }
    | 'ugt'            { AF.UGT }
    | 'uge'            { AF.UGE }
    | 'ult'            { AF.ULT }
    | 'ule'            { AF.ULE }
    | 'une'            { AF.UNE }
    | 'true'           { AF.True }

memoryOrdering :: { A.MemoryOrdering }
memoryOrdering :
    'unordered'        { A.Unordered }
  | 'monotonic'        { A.Monotonic }
  | 'acquire'          { A.Acquire }
  | 'release'          { A.Release }
  | 'acq_rel'          { A.AcquireRelease }
  | 'seq_cst'          { A.SequentiallyConsistent }

atomicity :: { A.Atomicity }
atomicity :
    'singlethread' memoryOrdering    { (A.SingleThread, $2) }
  | memoryOrdering                   { (A.System, $1) }

rmwOperation :: { AR.RMWOperation }
rmwOperation :
    'xchg'           { AR.Xchg }
  | 'add'            { AR.Add }
  | 'sub'            { AR.Sub }
  | 'and'            { AR.And }
  | 'nand'           { AR.Nand }
  | 'or'             { AR.Or }
  | 'xor'            { AR.Xor }
  | 'max'            { AR.Max }
  | 'min'            { AR.Min }
  | 'umax'           { AR.UMax }
  | 'umin'           { AR.UMin }

cleanup :: { Bool }
cleanup :
    {- empty -}        { False }
  | 'cleanup'          { True }

exact :: { Bool }
exact :
    {- empty -}        { False }
  | 'exact'            { True }

clause :: { A.LandingPadClause }
clause :
    'catch' tConstant     { A.Catch $2 }
  | 'filter' tConstant    { A.Filter $2 }

clauses :: { RevList A.LandingPadClause }
clauses :
    {- empty -}        { RNil }
  | clauses clause     { RCons $2 $1 }

phiItem :: { A.Type -> (A.Operand, A.Name) }
phiItem :
    '[' operand ',' name ']'     { \t -> ($2 t, $4) }

phiList :: { A.Type -> RevList (A.Operand, A.Name) }
phiList :
    phiItem                { \t -> RCons ($1 t) RNil }
  | phiList ',' phiItem    { \t -> RCons ($3 t) ($1 t) }

parameterAttribute :: { A.ParameterAttribute }
parameterAttribute :
    'zeroext'          { A.ZeroExt }
  | 'signext'          { A.SignExt }
  | 'inreg'            { A.InReg }
  | 'sret'             { A.SRet }
  | 'noalias'          { A.NoAlias }
  | 'byval'            { A.ByVal }
  | 'nocapture'        { A.NoCapture }
  | 'nest'             { A.Nest }

parameterAttributes :: { RevList A.ParameterAttribute }
parameterAttributes :
    {- empty -}                                { RNil }
  | parameterAttributes parameterAttribute     { RCons $2 $1 }

argument :: { (A.Type, (A.Operand, [A.ParameterAttribute])) }
argument :
    type parameterAttributes operand      { ($1, ($3 $1, rev $2)) }

argumentList_ :: { RevList (A.Type, (A.Operand, [A.ParameterAttribute])) }
argumentList_ :
    argument                          { RCons $1 RNil }
  | argumentList_ ',' argument        { RCons $3 $1 }

argumentList :: { RevList (A.Type, (A.Operand, [A.ParameterAttribute])) }
argumentList :
    {- empty -}                      { RNil }
  | argumentList_                    { $1 }

sideeffect :: { Bool }
sideeffect :
    {- empty -}       { False }
  | 'sideeffect'      { True }

alignstack :: { Bool }
alignstack :
    {- empty -}       { False }
  | 'alignstack'      { True }

dialect :: { A.Dialect }
dialect :
    {- empty -}       { A.ATTDialect }
  | 'inteldialect'    { A.IntelDialect }

callableOperand :: { [A.Type] -> A.CallableOperand }
callableOperand :
    type operand       { \ts -> Right ($2 (A.FunctionType $1 ts False)) }
  | type 'asm' sideeffect alignstack dialect STRING ',' STRING
                       { \ts -> Left (A.InlineAssembly (A.FunctionType $1 ts False) (fromString $6) (fromString $8) $3 $4 $5) }

tail :: { Maybe A.TailCallKind }
tail :
    {- empty -}          { Nothing }
  | 'tail'               { Just A.Tail }
  | 'musttail'           { Just A.MustTail }
  | 'notail'             {Just A.NoTail }

idx :: { Word32 }
idx :
    INT               { fromIntegral $1 }

idxs :: { RevList Word32 }
idxs :
    idx            { RCons $1 RNil }
  | idxs ',' idx   { RCons $3 $1 }

destination :: { (A.Constant, A.Name) }
destination :
    tConstant ',' label    { ($1, $3) }

destinations :: { RevList (A.Constant, A.Name) }
destinations :
    {- empty -}                   { RNil }
  | destinations destination      { RCons $2 $1 }

label :: { A.Name }
label :
  'label' name        { $2 }

labels :: { RevList A.Name }
labels :
    label                         { RCons $1 RNil }
  | labels ','label               { RCons $3 $1 }

metadata :: { A.Metadata }
metadata :
    metadataNode { A.MDNode $1 }
  | tOperand     { A.MDValue $1 }
  | '!' STRING   { A.MDString (fromString $2) }

metadataNodeID :: { A.MetadataNodeID }
metadataNodeID :
    UNNAMED_META       { A.MetadataNodeID $1 }

metadataNodeIDs :: { RevList A.MetadataNodeID }
metadataNodeIDs :
    metadataNodeID                      { RCons $1 RNil }
  | metadataNodeIDs ',' metadataNodeID  { RCons $3 $1 }

metadataNode :: { A.MetadataNode }
metadataNode :
    metadataNodeID         { A.MetadataNodeReference $1 }
  | '!' '{' metadataList '}'   { A.MetadataNode (rev $3) }

instructionMetaDataItem :: { (ShortByteString, A.MetadataNode) }
instructionMetaDataItem :
    ',' NAMED_META metadataNode   { ($2,$3) }

instructionMetadata :: { RevList (ShortByteString, A.MetadataNode) }
instructionMetadata :
    {- empty -}               { RNil }
  | instructionMetadata instructionMetaDataItem
                              { RCons $2 $1 }

instruction_ :: { A.InstructionMetadata -> A.Instruction }
instruction_ :
    'add' nuw nsw binOperands   { A.Add $3 $2 (fst $4) (snd $4) }
  | 'fadd' fmflags binOperands  { A.FAdd $2 (fst $3) (snd $3) }
  | 'sub' nuw nsw binOperands   { A.Sub $3 $2 (fst $4) (snd $4) }
  | 'fsub' fmflags binOperands  { A.FSub $2 (fst $3) (snd $3) }
  | 'mul' nuw nsw binOperands   { A.Mul $3 $2 (fst $4) (snd $4) }
  | 'fmul' fmflags binOperands  { A.FMul $2 (fst $3) (snd $3) }
  | 'udiv' exact binOperands    { A.UDiv $2 (fst $3) (snd $3) }
  | 'sdiv' exact binOperands    { A.SDiv $2 (fst $3) (snd $3) }
  | 'fdiv' fmflags binOperands  { A.FDiv $2 (fst $3) (snd $3) }
  | 'urem' binOperands          { A.URem (fst $2) (snd $2) }
  | 'srem' binOperands          { A.SRem (fst $2) (snd $2) }
  | 'frem' fmflags binOperands  { A.FRem $2 (fst $3) (snd $3) }
  | 'shl' nuw nsw binOperands   { A.Shl $3 $2 (fst $4) (snd $4) }
  | 'lshr' exact binOperands    { A.LShr $2 (fst $3) (snd $3) }
  | 'ashr' exact binOperands    { A.AShr $2 (fst $3) (snd $3) }
  | 'and' binOperands           { A.And (fst $2) (snd $2) }
  | 'or' binOperands            { A.Or (fst $2) (snd $2) }
  | 'xor' binOperands           { A.Xor (fst $2) (snd $2) }
  | 'alloca' type mOperand alignment        { A.Alloca $2 $3 $4 }
  | 'load' volatile tOperand alignment      { A.Load $2 $3 Nothing $4 }
  | 'load' 'atomic' volatile tOperand atomicity alignment      { A.Load $3 $4 (Just $5) $6 }
  | 'store' volatile tOperand ',' tOperand alignment
                                            { A.Store $2 $5 $3 Nothing $6 }
  | 'store' 'atomic' volatile tOperand ',' tOperand atomicity alignment
                                            { A.Store $3 $6 $4 (Just $7) $8 }
  | 'getelementptr' inBounds tOperand indices
                                            { A.GetElementPtr $2 $3 (rev $4) }
  | 'fence' atomicity                       { A.Fence $2 }
  | 'cmpxchg' volatile tOperand ',' tOperand ',' tOperand atomicity
                                            { A.CmpXchg $2 $3 $5 $7 $8 A.Unordered }
  | 'cmpxchg' volatile tOperand ',' tOperand ',' tOperand atomicity memoryOrdering
                                            { A.CmpXchg $2 $3 $5 $7 $8 $9 }
  | 'atomicrmw' volatile rmwOperation tOperand ',' tOperand atomicity
                                            { A.AtomicRMW $2 $3 $4 $6 $7 }
  | 'trunc' tOperand 'to' type              { A.Trunc $2 $4 }
  | 'zext' tOperand 'to' type               { A.ZExt $2 $4 }
  | 'sext' tOperand 'to' type               { A.SExt $2 $4 }
  | 'fptoui' tOperand 'to' type             { A.FPToUI $2 $4 }
  | 'fptosi' tOperand 'to' type             { A.FPToSI $2 $4 }
  | 'uitofp' tOperand 'to' type             { A.UIToFP $2 $4 }
  | 'sitofp' tOperand 'to' type             { A.SIToFP $2 $4 }
  | 'fptrunc' tOperand 'to' type            { A.FPTrunc $2 $4 }
  | 'fpext' tOperand 'to' type              { A.FPExt $2 $4 }
  | 'ptrtoint' tOperand 'to' type           { A.PtrToInt $2 $4 }
  | 'inttoptr' tOperand 'to' type           { A.IntToPtr $2 $4 }
  | 'bitcast' tOperand 'to' type            { A.BitCast $2 $4 }
  | 'addrspacecast' tOperand 'to' type      { A.AddrSpaceCast $2 $4 }
  | 'icmp' intP binOperands                 { A.ICmp $2 (fst $3) (snd $3) }
  | 'fcmp' fpP binOperands                  { A.FCmp $2 (fst $3) (snd $3) }
  | 'phi' type phiList                      { A.Phi $2 (rev ($3 $2)) }
  | tail 'call' cconv parameterAttributes callableOperand '(' argumentList ')' fAttributes
                                            { A.Call $1 $3 (rev $4) ($5 (map fst (rev $7))) (map snd (rev $7)) (map Right (rev $9)) }
  | 'select' tOperand ',' tOperand ',' tOperand
                                            { A.Select $2 $4 $6 }
  | 'va_arg' tOperand ',' type              { A.VAArg $2 $4 }
  | 'extractelement' tOperand ',' tOperand  { A.ExtractElement $2 $4 }
  | 'insertelement' tOperand ',' tOperand ',' tOperand
                                            { A.InsertElement $2 $4 $6 }
  | 'shufflevector' tOperand ',' tOperand ',' tConstant
                                            { A.ShuffleVector $2 $4 $6 }
  | 'extractvalue' tOperand ',' idxs        { A.ExtractValue $2 (rev $4) }
  | 'insertvalue' tOperand ',' tOperand ',' idxs
                                            { A.InsertValue $2 $4 (rev $6) }
  | 'landingpad' type cleanup clauses
                                            { A.LandingPad $2 $3 (rev $4) }
  | 'ret' 'void'                            { A.Ret Nothing }
  | 'ret' typeNoVoid operand                { A.Ret (Just ($3 $2)) }
  | 'br' 'label' name                       { A.Br $3 }
  | 'br' type operand ',' 'label' name ',' 'label' name
					    { A.CondBr ($3 $2) $6 $9 }
  | 'switch' type operand ',' 'label' name '[' destinations ']'
					    { A.Switch ($3 $2) $6 (rev $8) }
  | 'indirectbr' tOperand ',' '[' labels ']'
					    { A.IndirectBr $2 (rev $5) }
  | 'invoke' cconv parameterAttributes callableOperand '(' argumentList ')' fAttributes 'to' 'label' name 'unwind' 'label' name
                                            { A.Invoke $2 (rev $3) ($4 (map fst (rev $6))) (map snd (rev $6)) (map Right (rev $8)) $11 $14 }
  | 'resume' tOperand                       { A.Resume $2 }
  | 'unreachable'                           { A.Unreachable }
  | ANTI_INSTR                              {\[] -> A.AntiInstruction (fromString $1) }

instruction :: { A.Instruction }
instruction :
    instruction_ instructionMetadata   { $1 (rev $2) }
  | tOperand                           { A.OperandInstruction $1 }

name :: { A.Name }
name :
    NAMED_LOCAL     { A.Name $1 }
  | UNNAMED_LOCAL   { A.UnName $1 }
  | ANTI_ID         { A.AntiName (fromString $1) }

namedI :: { A.NamedInstruction }
namedI :
    instruction                     { A.Do $1 }
  | name '=' instruction            { $1 A.:= $3 }
  | ANTI_BB
      { A.AntiBasicBlock (fromString $1) }
  | ANTI_BBS
      { A.AntiBasicBlockList (fromString $1) }
  | ANTI_INSTRS                     { A.AntiInstructionList (fromString $1) }

elseInstrs :: { [A.LabeledInstruction] }
elseInstrs :
    {- empty -}         { [] }
  | 'else' '{' instructions '}'
      { rev $3 }

labeledI :: { A.LabeledInstruction }
labeledI :
    jumpLabel namedI                { A.Labeled $1 $2 }
  | jumpLabel 'for' type name 'in' operand direction operand mStep '{' instructions '}'
      { A.ForLoop $1 $3 $4 $7 ($6 $3) ($8 $3) ($9 $3) (rev $11) }
  | jumpLabel 'if' operand '{' instructions '}' elseInstrs
      { A.ITE $1 ($3 (A.IntegerType 1)) (rev $5) $7 }
  | jumpLabel 'while' operand '{' instructions '}'
      { A.While $1 ($3 (A.IntegerType 1)) (rev $5) }

instructions :: { RevList (A.LabeledInstruction) }
instructions :
    {- empty -}                   { RNil }
  | instructions labeledI         { RCons $2 $1 }

{------------------------------------------------------------------------------
 -
 - Basic Blocks
 -
 -----------------------------------------------------------------------------}

mStep :: { A.Type -> A.Operand }
mStep :
    {- empty -}        { A.ConstantOperand . intConstant 1 }
  | 'step' operand     { $2 }

jumpLabel :: { A.Name }
jumpLabel :
    JUMPLABEL           { A.Name (fromString $1) }
  | {- empty -}         { A.NeedsName }

direction :: { A.Direction }
direction :
    'to'        { A.Up }
  | 'downto'    { A.Down }

{------------------------------------------------------------------------------
 -
 - Global Definitions
 -
 -----------------------------------------------------------------------------}

globalName :: { A.Name }
globalName :
    NAMED_GLOBAL     { A.Name $1 }
  | UNNAMED_GLOBAL   { A.UnName $1 }
  | ANTI_GID         { A.AntiName (fromString $1) }

addrSpace :: { A.AddrSpace }
addrSpace :
    {- empty -}                { A.AddrSpace 0 }
  | 'addrspace' '(' INT ')'    { A.AddrSpace (fromIntegral $3) }

typeNoVoid :: { A.Type }
typeNoVoid :
    INTEGERTYPE               { A.IntegerType $1 }
  | 'half'                    { A.FloatingPointType LA.HalfFP }
  | 'float'                   { A.FloatingPointType LA.FloatFP }
  | 'double'                  { A.FloatingPointType LA.DoubleFP }
  | type addrSpace '*'        { A.PointerType $1 $2 }
  | type '(' typeListVar ')'  { A.FunctionType $1 (fst $3) (snd $3) }
  | '<' INT 'x' type '>'      { A.VectorType (fromIntegral $2) $4 }
  | '{' typeList '}'          { A.StructureType False (rev $2) }
  | '<' '{' typeList '}' '>'  { A.StructureType True (rev $3) }
  | '[' INT 'x' type ']'      { A.ArrayType (fromIntegral $2) $4 }
  | name                      { A.NamedTypeReference $1 }
  | 'metadata'                { A.MetadataType }
  | ANTI_TYPE                 { A.AntiType (fromString $1) }

type :: { A.Type }
type :
    'void'                    { A.VoidType }
  | typeNoVoid                { $1 }

mType :: { Maybe A.Type }
mType :
    type                 { Just $1 }
  | 'opaque'             { Nothing }

typeList_ :: { RevList A.Type }
typeList_ :
    type                             { RCons $1 RNil }
  | typeList_ ',' type               { RCons $3 $1 }

typeList :: { RevList A.Type }
typeList :
    {- empty -}                      { RNil }
  | typeList_                        { $1 }

typeListVar :: { ([A.Type], Bool) }
typeListVar :
    {- empty -}                      { ([], False) }
  | typeList_                        { (rev $1, False) }
  | '...'                            { ([], True) }
  | typeList_ ',' '...'              { (rev $1, True) }

linkage :: { A.Linkage }
linkage :
    {- empty -}                { A.External }
  | 'private'                  { A.Private }
  | 'internal'                 { A.Internal }
  | 'available_externally'     { A.AvailableExternally }
  | 'linkonce'                 { A.LinkOnce }
  | 'weak'                     { A.Weak }
  | 'common'                   { A.Common }
  | 'appending'                { A.Appending }
  | 'extern_weak'              { A.ExternWeak }
  | 'linkonce_odr'             { A.LinkOnceODR }
  | 'weak_odr'                 { A.WeakODR }
  | 'external'                 { A.External }

visibility :: { A.Visibility }
visibility :
    {- empty -}                { A.Default }
  | 'default'                  { A.Default }
  | 'hidden'                   { A.Hidden }
  | 'protected'                { A.Protected }

cconv :: { CC.CallingConvention }
cconv :
    {- empty -}                { CC.C }
  | 'ccc'                      { CC.C }
  | 'fastcc'                   { CC.Fast }
  | 'coldcc'                   { CC.Cold }
  | 'cc' INT                   { if $2 == 10 then CC.GHC else CC.Numbered (fromInteger $2) }

parameter :: { A.Parameter }
parameter :
    type parameterAttributes name { A.Parameter $1 $3 (rev $2) }
  | ANTI_PARAM                    { A.AntiParameter (fromString $1) }
  | ANTI_PARAMS                   { A.AntiParameterList (fromString $1) }

parameterList_ :: { RevList A.Parameter }
parameterList_ :
    parameter                        { RCons $1 RNil }
  | parameterList_ ',' parameter     { RCons $3 $1 }

parameterList :: { ([A.Parameter], Bool) }
parameterList :
    {- empty -}                      { ([], False) }
  | parameterList_                   { (rev $1, False) }
  | '...'                            { ([], True) }
  | parameterList_ ',' '...'         { (rev $1, True) }

parameterD :: { A.Parameter }
parameterD :
    type parameterAttributes { A.Parameter $1 (A.UnName 0) (rev $2) }

parameterListD_ :: { RevList A.Parameter }
parameterListD_ :
    parameterD                       { RCons $1 RNil }
  | parameterListD_ ',' parameterD   { RCons $3 $1 }

parameterListD :: { ([A.Parameter], Bool) }
parameterListD :
    {- empty -}                      { ([], False) }
  | parameterListD_                  { (rev $1, False) }
  | '...'                            { ([], True) }
  | parameterListD_ ',' '...'        { (rev $1, True) }

fAttribute :: { A.FunctionAttribute }
fAttribute :
    'alignstack' '(' INT ')'         { A.StackAlignment (fromIntegral $3) }
  | 'alwaysinline'                   { A.AlwaysInline }
  | 'inlinehint'                     { A.InlineHint }
  | 'naked'                          { A.Naked }
  | 'noimplicitfloat'                { A.NoImplicitFloat }
  | 'noinline'                       { A.NoInline }
  | 'nonlazybind'                    { A.NonLazyBind }
  | 'noredzone'                      { A.NoRedZone }
  | 'noreturn'                       { A.NoReturn }
  | 'nounwind'                       { A.NoUnwind }
  | 'optsize'                        { A.OptimizeForSize }
  | 'readnone'                       { A.ReadNone }
  | 'readonly'                       { A.ReadOnly }
  | 'ssp'                            { A.StackProtect }
  | 'sspreq'                         { A.StackProtectReq }
  | 'uwtable'                        { A.UWTable }

fAttributes :: { RevList A.FunctionAttribute }
fAttributes :
    {- empty -}                   { RNil }
  | fAttributes fAttribute        { RCons $2 $1 }

section :: { Maybe ShortByteString }
section :
    {- empty -}         { Nothing }
  | 'section' STRING    { Just (fromString $2) }

gc :: { Maybe ShortByteString }
gc :
    {- empty -}         { Nothing }
  | 'gc' STRING         { Just (fromString $2) }

isConstant :: { Bool }
isConstant :
    'global'        { False }
  | 'constant'      { True }

global :: { A.Global }
global :
    'define' linkage visibility cconv parameterAttributes type globalName '(' parameterList ')' fAttributes section alignment gc '{' instructions '}'
      { A.Function $2 $3 Nothing $4 (rev $5) $6 $7 $9 (map Right $ rev $11) $12 Nothing $13 $14 Nothing (rev $16) Nothing }
  | 'declare' linkage visibility cconv parameterAttributes type globalName '(' parameterListD ')' alignment gc
      { A.Function $2 $3 Nothing $4 (rev $5) $6 $7 $9 [] Nothing Nothing $11 $12 Nothing [] Nothing }
  | globalName '=' linkage visibility isConstant type mConstant alignment
      { A.GlobalVariable $1 $3 $4 Nothing Nothing Nothing $5 $6 (A.AddrSpace 0) ($7 $6) Nothing Nothing $8 }
  | globalName '=' visibility 'alias' linkage type constant
      { A.GlobalAlias $1 $5 $3 Nothing Nothing Nothing $6 (A.AddrSpace 0) ($7 $6) }

{------------------------------------------------------------------------------
 -
 - Definitions
 -
 -----------------------------------------------------------------------------}

metadataItem :: { Maybe A.Metadata }
metadataItem :
    metadata                    { Just $1 }
  | 'null'                      { Nothing }

metadataList_ :: { RevList (Maybe A.Metadata) }
metadataList_ :
    metadataItem                    { RCons $1 RNil }
  | metadataList_ ',' metadataItem  { RCons $3 $1 }

metadataList :: { RevList (Maybe A.Metadata) }
metadataList :
    {- empty -}                     { RNil }
  | metadataList_                   { $1 }

definition :: { A.Definition }
definition :
    global         { A.GlobalDefinition $1 }
  | name '=' 'type' mType
                   { A.TypeDefinition $1 $4 }
  | metadataNodeID '=' 'metadata' '!' '{' metadataList '}'
                   { A.MetadataNodeDefinition $1 (rev $6) }
  | NAMED_META '=' '!' '{' metadataNodeIDs '}'
                   { A.NamedMetadataDefinition $1 (rev $5) }
  | 'module' 'asm' STRING
                   { A.ModuleInlineAssembly (fromString $3) }
  | ANTI_DEF       { A.AntiDefinition (fromString $1) }
  | ANTI_DEFS      { A.AntiDefinitionList (fromString $1) }

definitions :: { RevList A.Definition }
definitions :
    {- empty -}             { RNil }
  | definitions definition  { RCons $2 $1 }

{------------------------------------------------------------------------------
 -
 - Modules
 -
 -----------------------------------------------------------------------------}

dataLayout :: { Maybe A.DataLayout }
dataLayout :
    {- empty -}                      { Nothing }
  | 'target' 'datalayout' '=' STRING { Just (dataLayout $4) }
  | ANTI_DL                          { Just (A.AntiDataLayout (fromString $1)) }

targetTriple :: { A.TargetTriple }
targetTriple :
    {- empty -}                       { A.NoTargetTriple }
  | 'target' 'triple' '=' STRING      { A.TargetTriple (fromString $4) }
  | ANTI_TT                           { A.AntiTargetTriple (fromString $1) }

module :: { A.Module }
module :
    dataLayout targetTriple definitions  { A.Module (fromString "<string>") (fromString "<string>") $1 $2 (rev $3) }

{
intConstant :: Integer -> A.Type -> A.Constant
intConstant n (A.IntegerType bs) = A.Int bs n
intConstant n (A.AntiType bs) = A.IntAntiBs bs n
intConstant n t = error $ "intConstant: unexpected type " ++ show t

floatConstant :: Rational -> A.Type -> A.Constant
floatConstant x (A.FloatingPointType LA.FloatFP) = A.Float (A.Single (fromRational x))
floatConstant x (A.FloatingPointType LA.DoubleFP) = A.Float (A.Double (fromRational x))

dataLayout :: String -> A.DataLayout
dataLayout s =
  case runExcept (A.parseDataLayout A.BigEndian (BS.pack s)) of
    Right (Just d) -> A.DataLayout d

happyError :: L T.Token -> P a
happyError (L loc t) =
    parserError (locStart loc) (text "parse error on" <+> quoteTok (ppr t))

lexer :: (L T.Token -> P a) -> P a
lexer cont = do
    t <- lexToken
    setCurToken t
    cont t

locate :: Loc -> (SrcLoc -> a) -> L a
locate loc f = L loc (f (SrcLoc loc))

data RevList a  =  RNil
                |  RCons a (RevList a)

rnil :: RevList a
rnil = RNil

rsingleton :: a -> RevList a
rsingleton x = RCons x RNil

rcons :: a -> RevList a -> RevList a
rcons x xs  = RCons x xs

rev :: RevList a -> [a]
rev xs = go [] xs
  where
    go  l  RNil          = l
    go  l  (RCons x xs)  = go (x : l) xs
}

-- vim: set filetype=happy:
