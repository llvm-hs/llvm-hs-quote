module LLVM.Quote.Parser.Tokens (
    Token(..),
    Visibility(..),
    Extensions(..),
    ExtensionsInt,
    keywords,
    keywordMap
  ) where

import qualified Data.Map as Map
import Data.Bits
import Data.Word
import Text.PrettyPrint.Mainland
import Data.List (foldl')

import LLVM.Prelude
import LLVM.Quote.AST

data Visibility
  = Global
  | Local
  | Meta
  deriving (Eq, Ord, Show)

data Token
  = Teof
  | TintConst Integer
  | TfloatConst Rational
  | TstringConst String
  | TcstringConst String
  | Tnamed Visibility ShortByteString
  | Tunnamed Visibility Word
  | TjumpLabel String
  | Tlparen
  | Trparen
  | Tlbrack
  | Trbrack
  | Tlbrace
  | Trbrace
  | Tlt
  | Tgt
  | Tcomma
  | Tassign
  | Tstar
  | Tminus
  | Tbang
  | Tpoints
  | Tx
  | Tzeroinitializer
  | Tundef
  | Tglobal
  | Tconstant
  | Talias
  | Tunwind
  | Tunordered
  | Tmonotonic
  | Tacquire
  | Trelease
  | Tacq_rel
  | Tseq_cst
  | Tsinglethread
  | Txchg
  | Tnand
  | Tmax
  | Tmin
  | Tumax
  | Tumin
  | Tcleanup
  | Tcatch
  | Tfilter
  | Tpersonality
  | Tprivate
  | Tinternal
  | Tavailable_externally
  | Tlinkonce
  | Tweak
  | Tcommon
  | Tappending
  | Textern_weak
  | Tlinkonce_odr
  | Tweak_odr
  | Texternal
  | Tdefault
  | Thidden
  | Tprotected
  | Tccc
  | Tfastcc
  | Tcoldcc
  | Tcc
  | Tatomic
  | Tnull
  | Texact
  | Taddrspace
  | Tblockaddress
  | Tmodule
  | Tasm
  | Ttype
  | Topaque
  | Tsideeffect
  | Tinteldialect
  | Tsection
  | Tgc
  | Ttail
  | Tmusttail
  | Tnotail
  -- Finalizer
  | Tret
  | Tbr
  | Tswitch
  | Tindirectbr
  | Tinvoke
  | Tresume
  | Tunreachable
  -- Operations
  | Tadd
  | Tfadd
  | Tsub
  | Tfsub
  | Tmul
  | Tfmul
  | Tudiv
  | Tsdiv
  | Tfdiv
  | Turem
  | Tsrem
  | Tfrem
  | Tshl
  | Tlshr
  | Tashr
  | Tand
  | Tor
  | Txor
  | Talloca
  | Tload
  | Tstore
  | Tgetelementptr
  | Tfence
  | Tcmpxchg
  | Tatomicrmw
  | Ttrunc
  | Tzext
  | Tsext
  | Tfptoui
  | Tfptosi
  | Tuitofp
  | Tsitofp
  | Tfptrunc
  | Tfpext
  | Tptrtoint
  | Tinttoptr
  | Tbitcast
  | Taddrspacecast
  | Ticmp
  | Tfcmp
  | Tphi
  | Tcall
  | Tselect
  | Tvaarg
  | Textractelement
  | Tinsertelement
  | Tshufflevector
  | Textractvalue
  | Tinsertvalue
  | Tlandingpad

  | Teq
  | Tne
  | Tugt
  | Tuge
  | Tult
  | Tule
  | Tsgt
  | Tsge
  | Tslt
  | Tsle
  | Tfalse
  | Toeq
  | Togt
  | Toge
  | Tolt
  | Tole
  | Tone
  | Tord
  | Tuno
  | Tueq
  | Tune
  | Ttrue

  | Tlabel
  | Tvolatile
  | Tinbounds
  | Talign
  | Tnnan
  | Tninf
  | Tnsz
  | Tarcp
  | Tfast
  | Tto
  | Tnsw
  | Tnuw

  | Ttarget
  | Tdatalayout
  | Ttriple
  | Tdefine
  | Tdeclare
  -- Types
  | Thalf
  | Tfloat
  | Tdouble
  | TintegerType Word32
  | Tvoid
  | Tmetadata
  -- Parameter Attributes
  | Tzeroext
  | Tsignext
  | Tinreg
  | Tbyval
  | Tsret
  | Tnoalias
  | Tnocapture
  | Tnest
  -- Function Attributes
  | Talignstack
  | Talwaysinline
  | Tinlinehint
  | Tnaked
  | Tnoimplicitfloat
  | Tnoinline
  | Tnonlazybind
  | Tnoredzone
  | Tnoreturn
  | Tnounwind
  | Toptsize
  | Treadnone
  | Treadonly
  | Tssp
  | Tsspreq
  | Tuwtable
  -- Anti-Quotation
  | Tanti_dl String
  | Tanti_tt String
  | Tanti_def String
  | Tanti_defs String
  | Tanti_bb String
  | Tanti_bbs String
  | Tanti_instr String
  | Tanti_instrs String
  | Tanti_type String
  | Tanti_opr String
  | Tanti_const String
  | Tanti_id String
  | Tanti_gid String
  | Tanti_param String
  | Tanti_params String
  deriving (Eq, Ord)

instance Show Token where
  show (TintConst _) = "INT"
  show (TfloatConst _) = "FLOAT"
  show (TstringConst _) = "STRING"
  show (TcstringConst _) = "CSTRING"
  show (Tnamed Global _) = "NAMED_GLOBAL"
  show (Tnamed Local _) = "NAMED_LOCAL"
  show (Tnamed Meta _) = "NAMED_META"
  show (Tunnamed Global _) = "UNNAMED_GLOBAL"
  show (Tunnamed Local _) = "UNNAMED_LOCAL"
  show (Tunnamed Meta _) = "UNNAMED_META"
  show (TjumpLabel _) = "JUMPLABEL"
  show (TintegerType _) = "INTEGERTYPE"
  show (Tanti_dl _) = "ANTI_DL"
  show (Tanti_tt _) = "ANTI_TT"
  show (Tanti_def _) = "ANTI_DEF"
  show (Tanti_defs _) = "ANTI_DEFS"
  show (Tanti_bb _) = "ANTI_BB"
  show (Tanti_bbs _) = "ANTI_BBS"
  show (Tanti_instr _) = "ANTI_INSTR"
  show (Tanti_instrs _) = "ANTI_INSTRS"
  show (Tanti_type _) = "ANTI_TYPE"
  show (Tanti_opr _) = "ANTI_OPR"
  show (Tanti_const _) = "ANTI_CONST"
  show (Tanti_id _) = "ANTI_ID"
  show (Tanti_gid _) = "ANTI_GID"
  show (Tanti_param _) = "ANTI_PARAM"
  show (Tanti_params _) = "ANTI_PARAMS"
  show Tlparen = "("
  show Trparen = ")"
  show Tlbrack = "["
  show Trbrack = "]"
  show Tlbrace = "{"
  show Trbrace = "}"
  show Tlt = "<"
  show Tgt = ">"
  show Tcomma = ","
  show Tassign = "="
  show Tstar = "*"
  show Tminus = "-"
  show Tbang = "!"
  show Tpoints = "..."
  show Tx = "x"
  show Tzeroinitializer = "zeroinitializer"
  show Tundef = "undef"
  show Tret = "ret"
  show Tbr = "br"
  show Tswitch = "switch"
  show Tindirectbr = "indirectbr"
  show Tinvoke = "invoke"
  show Tresume = "resume"
  show Tunreachable = "unreachable"
  show Tadd = "add"
  show Tfadd = "fadd"
  show Tsub = "sub"
  show Tfsub = "fsub"
  show Tmul = "mul"
  show Tfmul = "fmul"
  show Tudiv = "udiv"
  show Tsdiv = "sdiv"
  show Tfdiv = "fdiv"
  show Turem = "urem"
  show Tsrem = "srem"
  show Tfrem = "frem"
  show Tshl = "shl"
  show Tlshr = "lshr"
  show Tashr = "ashr"
  show Tand = "and"
  show Tor = "or"
  show Txor = "xor"
  show Talloca = "alloca"
  show Tload = "load"
  show Tstore = "store"
  show Tgetelementptr = "getelementptr"
  show Tfence = "fence"
  show Tcmpxchg = "cmpxchg"
  show Tatomicrmw = "atomicrmw"
  show Ttrunc = "trunc"
  show Tzext = "zext"
  show Tsext = "sext"
  show Tfptoui = "fptoui"
  show Tfptosi = "fptosi"
  show Tuitofp = "uitofp"
  show Tsitofp = "sitofp"
  show Tfptrunc = "fptrunc"
  show Tfpext = "fpext"
  show Tptrtoint = "ptrtoint"
  show Tinttoptr = "inttoptr"
  show Tbitcast = "bitcast"
  show Taddrspacecast = "addrspacecast"
  show Ticmp = "icmp"
  show Tfcmp = "fcmp"
  show Tphi = "phi"
  show Tcall = "call"
  show Tselect = "select"
  show Tvaarg = "va_arg"
  show Textractelement = "extractelement"
  show Tinsertelement = "insertelement"
  show Tshufflevector = "shufflevector"
  show Textractvalue = "extractvalue"
  show Tinsertvalue = "insertvalue"
  show Tlandingpad = "landingpad"
  show Teq = "eq"
  show Tne = "ne"
  show Tugt = "ugt"
  show Tuge = "uge"
  show Tult = "ult"
  show Tule = "ule"
  show Tsgt = "sgt"
  show Tsge = "sge"
  show Tslt = "slt"
  show Tsle = "sle"
  show Tfalse = "false"
  show Toeq = "oeq"
  show Togt = "ogt"
  show Toge = "oge"
  show Tolt = "olt"
  show Tole = "ole"
  show Tone = "one"
  show Tord = "ord"
  show Tuno = "uno"
  show Tueq = "ueq"
  show Tune = "une"
  show Ttrue = "true"
  show Tlabel = "label"
  show Tvolatile = "volatile"
  show Tinbounds = "inbounds"
  show Talign = "align"
  show Tnnan = "nnan"
  show Tninf = "ninf"
  show Tnsz = "nsz"
  show Tarcp = "arcp"
  show Tfast = "fast"
  show Tto = "to"
  show Tnsw = "nsw"
  show Tnuw = "nuw"
  show Ttarget = "target"
  show Tdatalayout = "datalayout"
  show Ttriple = "triple"
  show Tdefine = "define"
  show Tdeclare = "declare"
  show Tvoid = "void"
  show Thalf = "half"
  show Tfloat = "float"
  show Tdouble = "double"
  show Tmetadata = "metadata"
  show Tzeroext = "zeroext"
  show Tsignext = "signext"
  show Tinreg = "inreg"
  show Tbyval = "byval"
  show Tsret = "sret"
  show Tnoalias = "noalias"
  show Tnocapture = "nocapture"
  show Tnest = "nest"
  show Talignstack = "alignstack"
  show Talwaysinline = "alwaysinline"
  show Tinlinehint = "inlinehint"
  show Tnaked = "naked"
  show Tnoimplicitfloat = "noimplicitfloat"
  show Tnoinline = "noinline"
  show Tnonlazybind = "nonlazybind"
  show Tnoredzone = "noredzone"
  show Tnoreturn = "noreturn"
  show Tnounwind = "nounwind"
  show Toptsize = "optsize"
  show Treadnone = "readnone"
  show Treadonly = "readonly"
  show Tssp = "ssp"
  show Tsspreq = "sspreq"
  show Tuwtable = "uwtable"
  show Tglobal = "global"
  show Tconstant = "constant"
  show Talias = "alias"
  show Tunwind = "unwind"
  show Tunordered = "unordered"
  show Tmonotonic = "monotonic"
  show Tacquire = "acquire"
  show Trelease = "release"
  show Tacq_rel = "acq_rel"
  show Tseq_cst = "seq_cst"
  show Tsinglethread = "singlethread"
  show Txchg = "xchg"
  show Tnand = "nand"
  show Tmax = "max"
  show Tmin = "min"
  show Tumax = "umax"
  show Tumin = "umin"
  show Tcleanup = "cleanup"
  show Tcatch = "catch"
  show Tfilter = "filter"
  show Tpersonality = "personality"
  show Tprivate = "private"
  show Tinternal = "internal"
  show Tavailable_externally = "available_externall"
  show Tlinkonce = "linkonce"
  show Tweak = "weak"
  show Tcommon = "common"
  show Tappending = "appending"
  show Textern_weak = "extern_weak"
  show Tlinkonce_odr = "linkonce_odr"
  show Tweak_odr = "weak_odr"
  show Texternal = "external"
  show Tdefault = "default"
  show Thidden = "hidden"
  show Tprotected = "protected"
  show Tccc = "ccc"
  show Tfastcc = "fastcc"
  show Tcoldcc = "coldcc"
  show Tcc = "cc"
  show Tatomic = "atomic"
  show Tnull = "null"
  show Texact = "exact"
  show Taddrspace = "addrspace"
  show Tblockaddress = "blockaddress"
  show Tmodule = "module"
  show Tasm = "asm"
  show Ttype = "type"
  show Topaque = "opaque"
  show Tsideeffect = "sideeffect"
  show Tinteldialect = "inteldialect"
  show Tsection = "section"
  show Tgc = "gc"
  show Ttail = "tail"
  show Teof = "EOF"

instance Pretty Token where
    ppr = text . show

keywords :: [(String,             Token,            Maybe [Extensions])]
keywords = [("define",            Tdefine,          Nothing),
            ("declare",           Tdeclare,         Nothing),
            ("ret",               Tret,             Nothing),
            ("target",            Ttarget,          Nothing),
            ("datalayout",        Tdatalayout,      Nothing),
            ("triple",            Ttriple,          Nothing),
            ("float",             Tfloat,           Nothing),
            ("icmp",              Ticmp,            Nothing),
            ("add",               Tadd,             Nothing),
            ("fadd",              Tfadd,            Nothing),
            ("sub",               Tsub,             Nothing),
            ("fsub",              Tfsub,            Nothing),
            ("mul",               Tmul,             Nothing),
            ("fmul",              Tfmul,            Nothing),
            ("udiv",              Tudiv,            Nothing),
            ("sdiv",              Tsdiv,            Nothing),
            ("fdiv",              Tfdiv,            Nothing),
            ("urem",              Turem,            Nothing),
            ("srem",              Tsrem,            Nothing),
            ("frem",              Tfrem,            Nothing),
            ("shl",               Tshl,             Nothing),
            ("lshr",              Tlshr,            Nothing),
            ("ashr",              Tashr,            Nothing),
            ("and",               Tand,             Nothing),
            ("or",                Tor,              Nothing),
            ("xor",               Txor,             Nothing),
            ("alloca",            Talloca,          Nothing),
            ("load",              Tload,            Nothing),
            ("store",             Tstore,           Nothing),
            ("getelementptr",     Tgetelementptr,   Nothing),
            ("fence",             Tfence,           Nothing),
            ("cmpxchg",           Tcmpxchg,         Nothing),
            ("atomicrmw",         Tatomicrmw,       Nothing),
            ("trunc",             Ttrunc,           Nothing),
            ("zext",              Tzext,            Nothing),
            ("sext",              Tsext,            Nothing),
            ("fptoui",            Tfptoui,          Nothing),
            ("fptosi",            Tfptosi,          Nothing),
            ("uitofp",            Tuitofp,          Nothing),
            ("sitofp",            Tsitofp,          Nothing),
            ("fptrunc",           Tfptrunc,         Nothing),
            ("fpext",             Tfpext,           Nothing),
            ("ptrtoint",          Tptrtoint,        Nothing),
            ("inttoptr",          Tinttoptr,        Nothing),
            ("bitcast",           Tbitcast,         Nothing),
            ("addrspacecast",     Taddrspacecast,   Nothing),
            ("icmp",              Ticmp,            Nothing),
            ("fcmp",              Tfcmp,            Nothing),
            ("phi",               Tphi,             Nothing),
            ("call",              Tcall,            Nothing),
            ("select",            Tselect,          Nothing),
            ("va_arg",            Tvaarg,           Nothing),
            ("extractelement",    Textractelement,  Nothing),
            ("insertelement",     Tinsertelement,   Nothing),
            ("shufflevector",     Tshufflevector,   Nothing),
            ("extractvalue",      Textractvalue,    Nothing),
            ("insertvalue",       Tinsertvalue,     Nothing),
            ("landingpad",        Tlandingpad,      Nothing),
            ("ret",               Tret,             Nothing),
            ("br",                Tbr,              Nothing),
            ("switch",            Tswitch,          Nothing),
            ("indirectbr",        Tindirectbr,      Nothing),
            ("invoke",            Tinvoke,          Nothing),
            ("resume",            Tresume,          Nothing),
            ("unreachable",       Tunreachable,     Nothing),
            ("label",             Tlabel,           Nothing),
            ("volatile",          Tvolatile,        Nothing),
            ("inbounds",          Tinbounds,        Nothing),
            ("align",             Talign,           Nothing),
            ("nnan",              Tnnan,            Nothing),
            ("ninf",              Tninf,            Nothing),
            ("nsz",               Tnsz,             Nothing),
            ("arcp",              Tarcp,            Nothing),
            ("fast",              Tfast,            Nothing),
            ("eq",                Teq,              Nothing),
            ("ne",                Tne,              Nothing),
            ("ugt",               Tugt,             Nothing),
            ("uge",               Tuge,             Nothing),
            ("ult",               Tult,             Nothing),
            ("ule",               Tule,             Nothing),
            ("sgt",               Tsgt,             Nothing),
            ("sge",               Tsge,             Nothing),
            ("slt",               Tslt,             Nothing),
            ("sle",               Tsle,             Nothing),
            ("false",             Tfalse,           Nothing),
            ("oeq",               Toeq,             Nothing),
            ("ogt",               Togt,             Nothing),
            ("oge",               Toge,             Nothing),
            ("olt",               Tolt,             Nothing),
            ("ole",               Tole,             Nothing),
            ("one",               Tone,             Nothing),
            ("ord",               Tord,             Nothing),
            ("uno",               Tuno,             Nothing),
            ("ueq",               Tueq,             Nothing),
            ("une",               Tune,             Nothing),
            ("true",              Ttrue,            Nothing),
            ("to",                Tto,              Nothing),
            ("nsw",               Tnsw,             Nothing),
            ("nuw",               Tnuw,             Nothing),
            ("zeroext",           Tzeroext,         Nothing),
            ("signext",           Tsignext,         Nothing),
            ("inreg",             Tinreg,           Nothing),
            ("byval",             Tbyval,           Nothing),
            ("sret",              Tsret,            Nothing),
            ("noalias",           Tnoalias,         Nothing),
            ("nest",              Tnest,            Nothing),
            ("x",                 Tx,               Nothing),
            ("zeroinitializer",   Tzeroinitializer, Nothing),
            ("undef",             Tundef,           Nothing),
            ("nounwind",          Tnounwind,        Nothing),
            ("nocapture",         Tnocapture,       Nothing),
            ("double",            Tdouble,          Nothing),
            ("float",             Tfloat,           Nothing),
            ("half",              Thalf,            Nothing),
            ("void",              Tvoid,            Nothing),
            ("metadata",          Tmetadata,        Nothing),
            ("alignstack",        Talignstack,      Nothing),
            ("alwaysinline",      Talwaysinline,    Nothing),
            ("inlinehint",        Tinlinehint,      Nothing),
            ("naked",             Tnaked,           Nothing),
            ("noimplicitfloat",   Tnoimplicitfloat, Nothing),
            ("noinline",          Tnoinline,        Nothing),
            ("nonlazybind",       Tnonlazybind,     Nothing),
            ("noredzone",         Tnoredzone,       Nothing),
            ("noreturn",          Tnoreturn,        Nothing),
            ("nounwind",          Tnounwind,        Nothing),
            ("optsize",           Toptsize,         Nothing),
            ("readnone",          Treadnone,        Nothing),
            ("readonly",          Treadonly,        Nothing),
            ("ssp",               Tssp,             Nothing),
            ("sspreq",            Tsspreq,          Nothing),
            ("uwtable",           Tuwtable,         Nothing),
            ("global",            Tglobal,          Nothing),
            ("constant",          Tconstant,        Nothing),
            ("alias",             Talias,           Nothing),
            ("unwind",            Tunwind,          Nothing),
            ("unordered",         Tunordered,       Nothing),
            ("monotonic",         Tmonotonic,       Nothing),
            ("acquire",           Tacquire,         Nothing),
            ("release",           Trelease,         Nothing),
            ("acq_rel",           Tacq_rel,         Nothing),
            ("seq_cst",           Tseq_cst,         Nothing),
            ("singlethread",      Tsinglethread,    Nothing),
            ("xchg",              Txchg,            Nothing),
            ("nand",              Tnand,            Nothing),
            ("max",               Tmax,             Nothing),
            ("min",               Tmin,             Nothing),
            ("umax",              Tumax,            Nothing),
            ("umin",              Tumin,            Nothing),
            ("cleanup",           Tcleanup,         Nothing),
            ("catch",             Tcatch,           Nothing),
            ("filter",            Tfilter,          Nothing),
            ("personality",       Tpersonality,     Nothing),
            ("private",           Tprivate,         Nothing),
            ("internal",          Tinternal,        Nothing),
            ("available_externally",
                                  Tavailable_externally,
                                                    Nothing),
            ("linkonce",          Tlinkonce,        Nothing),
            ("weak",              Tweak,            Nothing),
            ("common",            Tcommon,          Nothing),
            ("appending",         Tappending,       Nothing),
            ("extern_weak",       Textern_weak,     Nothing),
            ("linkonce_odr",      Tlinkonce_odr,    Nothing),
            ("weak_odr",          Tweak_odr,        Nothing),
            ("external",          Texternal,        Nothing),
            ("default",           Tdefault,         Nothing),
            ("hidden",            Thidden,          Nothing),
            ("protected",         Tprotected,       Nothing),
            ("ccc",               Tccc,             Nothing),
            ("fastcc",            Tfastcc,          Nothing),
            ("coldcc",            Tcoldcc,          Nothing),
            ("cc",                Tcc,              Nothing),
            ("atomic",            Tatomic,          Nothing),
            ("null",              Tnull,            Nothing),
            ("exact",             Texact,           Nothing),
            ("addrspace",         Taddrspace,       Nothing),
            ("blockaddress",      Tblockaddress,    Nothing),
            ("module",            Tmodule,          Nothing),
            ("asm",               Tasm,             Nothing),
            ("type",              Ttype,            Nothing),
            ("opaque",            Topaque,          Nothing),
            ("sideeffect",        Tsideeffect,      Nothing),
            ("inteldialect",      Tinteldialect,    Nothing),
            ("section",           Tsection,         Nothing),
            ("gc",                Tgc,              Nothing),
            ("tail",              Ttail,            Nothing)
           ]

keywordMap :: Map.Map String (Token, Maybe ExtensionsInt)
keywordMap = Map.fromList (map f keywords)
  where
    f  ::  (String, Token, Maybe [Extensions])
       ->  (String, (Token, Maybe ExtensionsInt))
    f (s, t, Nothing)    = (s, (t, Nothing))
    f (s, t, Just exts)  = (s, (t, Just i))
      where
        i = foldl' setBit 0 (map fromEnum exts)
