{-# LANGUAGE NamedFieldPuns #-}

module Example.Project
    ( topEntity, plus,
      execute, cpuInit,
      opParse, aluOpParse  -- Tests
    ) where

import Clash.Prelude
import Prelude ( foldl )

data Stack = Stack (Signed 5) (Vec 32 (Signed 16))
    deriving Show
type Addr    = Signed 13
type Literal = Signed 16
type Instr   = Signed 16

data CpuState = CpuState
    { ip :: Addr
    , ds :: Stack
    , rs :: Stack
    }
    deriving Show

stackInit :: Stack
stackInit = Stack 0 $ replicate d32 0

cpuInit :: CpuState
cpuInit   = CpuState {ip=0, ds=stackInit, rs=stackInit}

tos :: Stack -> Literal
tos (Stack sp s) = s!!(max 0 sp-1)

push :: Signed 16 -> Stack -> Stack
push v (Stack sp s) = Stack (sp+1) $ replace sp v s

pop :: Stack -> (Stack, Literal)
pop (Stack sp s) = (Stack (sp-1) s, s!!(sp-1))

type StackDelta = Signed 2

data AluFlags = AluFlags
    { rpc :: BitVector 1
    , tn  :: BitVector 1
    , tr  :: BitVector 1
    , nta :: BitVector 1
    } deriving (Eq, Show)

data T' = T
        | N
        | ADD
        | AND
        | OR
        | XOR
        | INV
        | EQUAL
        | CMP
        | RSH
        | DEC
        | R
        | Taddr
        | LSH
        | Depth
        | ULT
    deriving (Enum, Eq, Show)


data Op = NOP  -- Not implemented by the CPU but it's handy
        | JMP Addr
        | JZ Addr
        | CALL Addr
        | LIT (Signed 16)
        | AluOp T' AluFlags StackDelta StackDelta
    deriving (Eq, Show)

aluOpParse :: Signed 13 -> Op
aluOpParse $(bitPattern "abbbbcde.ffgg") = AluOp (toEnum $ unpack $ resize bbbb)
                                                 (AluFlags {rpc=a, tn=c, tr=d, nta=e})
                                                 (unpack ff)
                                                 (unpack gg)
aluOpParse _ = AluOp T (AluFlags {rpc=0, tn=0, tr=0, nta=0}) 0 0  -- No-op

opParse :: Instr -> Op
opParse w@($(bitPattern "1...............")) = LIT  $ 0x7FFF .&. w
opParse w@($(bitPattern "000.............")) = JMP  $ getAddr w
opParse w@($(bitPattern "001.............")) = JZ   $ getAddr w
opParse w@($(bitPattern "010.............")) = CALL $ getAddr w
opParse w@($(bitPattern "011.............")) = aluOpParse $ resize w
opParse _                                    = NOP

getAddr :: Instr -> Addr
getAddr = resize . (0x1FFF .&.)


alu :: CpuState -> Op -> CpuState
alu (CpuState ip (Stack dsp ds) (Stack rsp rs)) (AluOp op (AluFlags rpc tn tr nta) dd rd) =
    CpuState ip' (Stack dsp' ds') (Stack rsp' rs')
    where dsp' = dsp + resize dd
          rsp' = rsp + resize rd
          ip'  = if rpc == 1 then resize $ rs'!!rsp'
                             else ip+1
          rs'  = if tr  == 1 then replace rsp' (rs!!rsp) rs
                             else rs
          ds'  = replace dsp' t' ds
          t'   = case op of
              T     -> ds!!dsp
              N     -> ds!!(dsp-1)
              ADD   -> ds!!(dsp-1)  +  ds!!dsp
              AND   -> ds!!(dsp-1) .&. ds!!dsp
              OR    -> ds!!(dsp-1) .|. ds!!dsp
              XOR   -> ds!!(dsp-1) .|. ds!!dsp
              INV   -> -(ds!!dsp)
              EQUAL -> bool $ ds!!(dsp-1) == ds!!dsp
              CMP   -> bool $ ds!!(dsp-1) <  ds!!dsp
              RSH   -> (ds!!(dsp-1)) `shiftR` fromIntegral (ds!!dsp)
              DEC   -> ds!!dsp - 1
              R     -> rs!!rsp
              Taddr -> error "to be implemented"
              LSH   -> (ds!!(dsp-1)) `shiftL` fromIntegral (ds!!dsp)
              Depth -> resize dsp
              ULT   -> bool $ us (ds!!(dsp-1)) < us (ds!!dsp)
          bool cond = if cond then -1 else 0
          us :: KnownNat a => Signed a -> Unsigned a
          us = bitCoerce
alu _ _ = error "Invalid Op passed to @alu@"

cpu :: CpuState -> Op -> CpuState
cpu state op = case op of
    NOP       -> incIP
    JMP  addr -> setIP addr
    JZ   addr -> if t == 0 then setIP addr
                           else incIP
    CALL addr -> (setIP addr) {rs = push (resize $ ip state+1) (rs state)}
    LIT  v    -> incIP {ds = push v (ds state)}
    _         -> alu state op  -- nop
    where incIP   = state {ip = ip state + 1}
          setIP a = state {ip = a}
          t       = tos $ ds state


type Prog = [Op]

execute :: Prog -> CpuState
execute = Prelude.foldl cpu cpuInit

-- | Add two numbers. Example:
--
-- >>> plus 3 5
-- 8
plus :: Signed 8 -> Signed 8 -> Signed 8
plus a b = a + b

-- | 'topEntity' is Clash's equivalent of 'main' in other programming
-- languages. Clash will look for it when compiling 'Example.Project'
-- and translate it to HDL. While polymorphism can be used freely in
-- Clash projects, a 'topEntity' must be monomorphic and must use non-
-- recursive types. Or, to put it hand-wavily, a 'topEntity' must be
-- translatable to a static number of wires.
topEntity :: Signed 8 -> Signed 8 -> Signed 8
topEntity = plus
