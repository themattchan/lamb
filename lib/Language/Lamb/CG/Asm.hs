module Language.Lamb.CG.Asm (asm) where

import           Data.Monoid
import qualified Data.List as L
import           Text.Printf (printf)
import           Language.Lamb.Types

--------------------------------------------------------------------------------
-- | Convert a sequence of x86 `Instructions` into the output assembly
--------------------------------------------------------------------------------
asm :: [Instruction] -> Text
--------------------------------------------------------------------------------
asm instrs = header
          <> instrsAsm (prelude ++ instrs ++ postlude)
          <> "\n"

instrsAsm :: [Instruction] -> Text
instrsAsm = L.intercalate "\n" . map instrAsm

header :: Text
header = unlines
  [ "section .text"
  , "extern error"
  , "extern print"
  , "extern equal"
  , "extern try_gc"
  , "extern HEAP"
  , "extern HEAP_END"
  , "extern STACK_BOTTOM"
  , "global our_code_starts_here"
  ]

prelude :: [Instruction]
prelude =
  [ ILabel (Builtin "our_code_starts_here")

    -- init ESI with HEAP(START)
  , IMov (Reg ESI) (RegOffset 4 ESP)

  -- ensure 8-byte alignment (done by main.c)
  , IAdd (Reg ESI) (Const 8)
  , IAnd (Reg ESI) (HexConst 0xFFFFFFF8)

    -- init ACTUAL HEAP start
  , IMov (LabelVar "HEAP") (Reg ESI)

    -- init STACK_BOTTOM, used by gc
  , IMov (LabelVar "STACK_BOTTOM") (Reg ESP)
  ]

postlude :: [Instruction]
postlude = concat
  [ dynError (TypeError TNumber)
  , dynError (TypeError TBoolean)
  , dynError (TypeError TTuple)
  , dynError (ArithOverflow)
  , dynError (IndexLow)
  , dynError (IndexHigh)
  ]

dynError   :: DynError -> [Instruction]
dynError e =
  [ ILabel (DynamicErr e)
  , ISub   (Reg ESP) (Const 8)
  , IPush  (Reg EAX)
  , IPush  (Const (errorCode e))
  , ICall  (Builtin "error")
  ]


errorCode :: DynError -> Int
errorCode (TypeError TNumber)  = 0
errorCode (TypeError TBoolean) = 1
errorCode (ArithOverflow)      = 2
errorCode (TypeError TTuple)   = 3
errorCode (IndexLow)           = 4
errorCode (IndexHigh)          = 5

--------------------------------------------------------------------------------
instrAsm :: Instruction -> Text
--------------------------------------------------------------------------------
instrAsm (IMov dst val) = printf "  mov %s, %s"  (argAsm dst) (argAsm val)
instrAsm (IAdd dst val) = printf "  add %s, %s"  (argAsm dst) (argAsm val)
instrAsm (ISub dst val) = printf "  sub %s, %s"  (argAsm dst) (argAsm val)
instrAsm (IMul dst val) = printf "  imul %s, %s" (argAsm dst) (argAsm val)
instrAsm (IAnd dst msk) = printf "  and %s, %s"  (argAsm dst) (argAsm msk)
instrAsm (IOr  dst msk) = printf "  or  %s, %s"  (argAsm dst) (argAsm msk)
instrAsm (IXor dst msk) = printf "  xor %s, %s"  (argAsm dst) (argAsm msk)
instrAsm (IShl dst val) = printf "  shl %s, %s"  (argAsm dst) (argAsm val)
instrAsm (IShr dst val) = printf "  shr %s, %s"  (argAsm dst) (argAsm val)
instrAsm (ISar dst val) = printf "  sar %s, %s"  (argAsm dst) (argAsm val)
instrAsm (ICmp a1 a2)   = printf "  cmp %s, %s"  (argAsm a1)  (argAsm a2)
instrAsm (IPush a)      = printf "  push %s"     (argAsm a)
instrAsm (IPop a)       = printf "  pop  %s"     (argAsm a)
instrAsm (ICall l)      = printf "  call %s"     (labelAsm l)
instrAsm (ILabel l)     = printf "%s: "          (labelAsm l)
instrAsm (IJe  l)       = printf "  je  near %s" (labelAsm l)
instrAsm (IJne  l)      = printf "  jne near %s" (labelAsm l)
instrAsm (IJg  l)       = printf "  jg  near %s" (labelAsm l)
instrAsm (IJl  l)       = printf "  jl  near %s" (labelAsm l)
instrAsm (IJo  l)       = printf "  jo  near %s" (labelAsm l)
instrAsm (IJmp l)       = printf "  jmp near %s" (labelAsm l)
instrAsm IRet           =        "  ret"



regAsm :: Reg -> Text
regAsm EAX = "eax"
regAsm EBX = "ebx"
regAsm ESP = "esp"
regAsm EBP = "ebp"
regAsm ESI = "esi"

sizeAsm :: Size -> Text
sizeAsm DWordPtr = "DWORD"
sizeAsm WordPtr  = "WORD"
sizeAsm BytePtr  = "BYTE"

argAsm :: Arg -> Text
argAsm (Const n)       = printf "%d" n
argAsm (HexConst n)    = printf "0x%X" n
argAsm (Reg r)         = regAsm r
argAsm (RegOffset n r)
  | 0 <= n             = printf "[%s + %d]" (regAsm r) n
  | otherwise          = printf "[%s - %d]" (regAsm r) (0 - n)
argAsm (RegIndex r i)  = printf "[%s + %s * 4]" (regAsm r) (regAsm i)
argAsm (Sized s a)     = printf "%s %s"     (sizeAsm s) (argAsm a)
argAsm (LabelVar x)    = printf "[%s]"      x

labelAsm :: Label -> Text
labelAsm (BranchTrue i) = printf "label_%d_true"         i
labelAsm (BranchDone i) = printf "label_%d_done"         i
labelAsm (MemCheck   i) = printf "label_memcheck_%d"     i
labelAsm (DefStart f i) = printf "label_%d_def_%s_start" i f
labelAsm (DefEnd   f i) = printf "label_%d_def_%s_start" i f
labelAsm (DynamicErr e) = dynErrorLabel e
labelAsm (Builtin f)    = f

_idLabel :: Id -> Text
_idLabel f = "fun_" ++ f

dynErrorLabel :: DynError -> Text
dynErrorLabel (TypeError t)   = errorLabel ("non_" <> pprint t)
dynErrorLabel (ArithOverflow) = errorLabel "overflow"
dynErrorLabel (IndexLow)      = errorLabel "index_too_low"
dynErrorLabel (IndexHigh)     = errorLabel "index_too_high"

errorLabel :: Text -> Text
errorLabel l = "internal_error_" <> l -- errorFun l
