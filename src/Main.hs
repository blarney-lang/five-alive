-- Sample instantiation of the Five pipeline to produce an RV32I
-- microcontroller with a lone UART for I/O.

module Main where

-- General imports
import Data.Proxy

-- Blarney imports
import Blarney
import Blarney.Queue
import Blarney.Option
import Blarney.Stream
import Blarney.BitScan
import Blarney.SourceSink
import Blarney.ClientServer

-- Five imports
import Blarney.Five

-- Local imports
import FiveAlive.JTAGUART

-- RV32I parameters
-- ================

-- Register width
type XLen = 32

-- Instruction width
type ILen = 32

-- Raw instruction
type RawInstr = Bit ILen

-- Log of number of registers
type LogRegs = 5

-- Register identifier
type RegId = Bit LogRegs

-- RV32I instruction mnemonics
-- ===========================

data Mnemonic =
    LUI
  | AUIPC
  | ADD
  | SLT
  | SLTU
  | AND
  | OR
  | XOR
  | SLL
  | SRL
  | SRA
  | SUB
  | JAL
  | JALR
  | BEQ
  | BNE
  | BLT
  | BLTU
  | BGE
  | BGEU
  | LOAD
  | STORE
  | FENCE
  | ECALL
  | EBREAK
  | CSRRW
  | CSRRS
  | CSRRC
  deriving (Bounded, Enum, Show, Ord, Eq)

-- | Upper bound on number of instruction mnemonics used by the decoder
type MaxMnemonics = 64

-- | Bit vector indentifying one or more active mnemonics
type MnemonicVec = Bit MaxMnemonics

-- Checking if any of the given mnemonics are active
infix 8 `is`
is :: MnemonicVec -> [Mnemonic] -> Bit 1
is vec ms = orList [unsafeAt (fromEnum m) vec | m <- ms]

-- RV32I instruction decode table
-- ==============================

decodeTable =
  [ "imm[31:12] rd<5> 0110111" --> LUI
  , "imm[31:12] rd<5> 0010111" --> AUIPC
  , "imm[11:0] rs1<5> 000 rd<5> 0010011" --> ADD
  , "imm[11:0] rs1<5> 010 rd<5> 0010011" --> SLT
  , "imm[11:0] rs1<5> 011 rd<5> 0010011" --> SLTU
  , "imm[11:0] rs1<5> 111 rd<5> 0010011" --> AND
  , "imm[11:0] rs1<5> 110 rd<5> 0010011" --> OR
  , "imm[11:0] rs1<5> 100 rd<5> 0010011" --> XOR
  , "0000000 imm[4:0] rs1<5> 001 rd<5> 0010011" --> SLL
  , "0000000 imm[4:0] rs1<5> 101 rd<5> 0010011" --> SRL
  , "0100000 imm[4:0] rs1<5> 101 rd<5> 0010011" --> SRA
  , "0000000 rs2<5> rs1<5> 000 rd<5> 0110011" --> ADD
  , "0000000 rs2<5> rs1<5> 010 rd<5> 0110011" --> SLT
  , "0000000 rs2<5> rs1<5> 011 rd<5> 0110011" --> SLTU
  , "0000000 rs2<5> rs1<5> 111 rd<5> 0110011" --> AND
  , "0000000 rs2<5> rs1<5> 110 rd<5> 0110011" --> OR
  , "0000000 rs2<5> rs1<5> 100 rd<5> 0110011" --> XOR
  , "0100000 rs2<5> rs1<5> 000 rd<5> 0110011" --> SUB
  , "0000000 rs2<5> rs1<5> 001 rd<5> 0110011" --> SLL
  , "0000000 rs2<5> rs1<5> 101 rd<5> 0110011" --> SRL
  , "0100000 rs2<5> rs1<5> 101 rd<5> 0110011" --> SRA
  , "imm[20] imm[10:1] imm[11] imm[19:12] rd<5> 1101111" --> JAL
  , "imm[11:0] rs1<5> 000 rd<5> 1100111" --> JALR
  , "off[12] off[10:5] rs2<5> rs1<5> 000 off[4:1] off[11] 1100011" --> BEQ
  , "off[12] off[10:5] rs2<5> rs1<5> 001 off[4:1] off[11] 1100011" --> BNE
  , "off[12] off[10:5] rs2<5> rs1<5> 100 off[4:1] off[11] 1100011" --> BLT
  , "off[12] off[10:5] rs2<5> rs1<5> 110 off[4:1] off[11] 1100011" --> BLTU
  , "off[12] off[10:5] rs2<5> rs1<5> 101 off[4:1] off[11] 1100011" --> BGE
  , "off[12] off[10:5] rs2<5> rs1<5> 111 off[4:1] off[11] 1100011" --> BGEU
  , "000000000000 <5> 000 <5> 1110011" --> ECALL
  , "000000000001 <5> 000 <5> 1110011" --> EBREAK
  , "fence<4> pred<4> succ<4> rs1<5> 000 00000 0001111" --> FENCE
  , "csr[11:0] rs1<5> csrI<1> 01 rd<5> 1110011" --> CSRRW
  , "csr[11:0] rs1<5> csrI<1> 10 rd<5> 1110011" --> CSRRS
  , "csr[11:0] rs1<5> csrI<1> 11 rd<5> 1110011" --> CSRRC
  , "imm[11:0] rs1<5> ul<1> aw<2> rd<5> 0000011" --> LOAD
  , "imm[11:5] rs2<5> rs1<5> 0 aw<2> imm[4:0] 0100011" --> STORE
  ]

-- Decoded instruction format
-- ==========================

-- Decoded instruction
data Instr =
  Instr {
    rd          :: Option RegId
  , rs1         :: Option RegId
  , rs2         :: Option RegId
  , imm         :: Option (Bit XLen)
  , opcode      :: MnemonicVec
  , off         :: Bit 13
  , csr         :: Bit 12
  , csrI        :: Bit 1
  , accessWidth :: Bit 2
  , isUnsigned  :: Bit 1
  , isMemAccess :: Bit 1
  }
  deriving (Generic, Bits)

-- Instruction decoder
decodeInstr :: RawInstr -> Instr
decodeInstr instr =
  Instr {
    rd          = Option (hasBitField fieldMap "rd" .&&. regDest .!=. 0)
                         regDest               -- Never write register 0
  , rs1         = Option (hasBitField fieldMap "rs1")
                         (getBitFieldSel selMap "rs1" instr)
  , rs2         = Option (hasBitField fieldMap "rs2")
                         (getBitFieldSel selMap "rs2" instr)
  , imm         = getBitField fieldMap "imm"
  , opcode      = opcode
  , off         = getBitFieldSel selMap "off" instr
  , csr         = getBitFieldSel selMap "csr" instr
  , csrI        = getBitFieldSel selMap "csrI" instr
  , accessWidth = getBitFieldSel selMap "aw" instr
  , isUnsigned  = getBitFieldSel selMap "ul" instr
  , isMemAccess = opcode `is` [LOAD, STORE, FENCE]
  }
  where
    regDest            = getBitFieldSel selMap "rd" instr :: RegId
    (tagMap, fieldMap) = matchMap False decodeTable instr
    selMap             = matchSel decodeTable
    opcode             = packTagMap tagMap

-- Memory request format
-- =====================

-- Memory operation type
data MemOp = MemOp (Bit 1)
  deriving (Generic, Bits, Cmp)

-- Memory operation data constructors
memLoadOp  = MemOp 0
memStoreOp = MemOp 1

-- Memory request type
data MemReq =
  MemReq {
    -- Memory operation
    op :: MemOp
    -- Memory address
  , addr :: Bit XLen
    -- Data payload
  , payload :: Bit XLen
    -- Is value being loaded unsigned?
  , isUnsigned  :: Bit 1
    -- Access width
  , accessWidth :: Bit 2
  }
  deriving (Generic, Bits)

-- Add/subtract/compare unit
-- =========================

-- Inputs
data AddIns =
  AddIns {
    -- Are the operands unsigned?
    addUnsigned :: Bit 1
    -- Do subtraction/comparison rather than addition?
  , addSub :: Bit 1
    -- Operands
  , addOpA :: Bit 32
  , addOpB :: Bit 32
  }

-- Outputs
data AddOuts =
  AddOuts {
    -- Output of adder
    addSum :: Bit 32
    -- Result of comparison (assuming addSub was true)
  , addLessThan :: Bit 1
  , addEqual :: Bit 1
  }

-- Add/sub/compare unit
addUnit :: AddIns -> AddOuts
addUnit ins =
    AddOuts {
      addSum = truncate sum
    , addLessThan = at @32 sum
    , addEqual = a .==. b
    }
  where
    u = ins.addUnsigned
    a = ins.addOpA
    b = ins.addOpB
    isSub = ins.addSub
    addA = (if u then 0 else at @31 a) # a
    addB = (if u then 0 else at @31 b) # b
    sum = addA + (if isSub then inv addB else addB)
               + (if isSub then 1 else 0)

-- RV32I CSR unit
-- ==============

-- Interface to RV32I control/status registers
data CSRUnit =
  CSRUnit {
    -- Read given CSR
    read :: Bit 12 -> Action (Bit XLen)
    -- Write given CSR with given value
  , write :: Bit 12 -> Bit XLen -> Action ()
  }

-- RV32I instruction set
-- =====================

-- Instruction set interface
instrSet :: CSRUnit -> InstrSet XLen ILen Instr LogRegs MemReq
instrSet csrUnit =
  InstrSet {
    getDest      = \i -> i.rd
  , getSrcs      = \i -> [i.rs1, i.rs2]
  , numSrcs      = 2
  , isMemAccess  = \i -> i.isMemAccess
  , decode       = decodeInstr
  , makeExecUnit = makeRV32IExecUnit csrUnit
  }

-- Execution unit
makeRV32IExecUnit :: CSRUnit -> Module (ExecUnit XLen Instr MemReq)
makeRV32IExecUnit csrUnit = return (ExecUnit issue)
  where
    issue instr s = do
      -- Operands
      let [opr1, opr2] = s.operands

      -- Second operand
      let opr2orImm = if instr.imm.valid then instr.imm.val else opr2

      -- Add/sub/compare unit
      let AddOuts sum less equal = addUnit 
            AddIns {
              addSub = inv (instr.opcode `is` [ADD])
            , addUnsigned = instr.opcode `is` [SLTU, BLTU, BGEU]
            , addOpA = opr1
            , addOpB = opr2orImm
            }

      when (instr.opcode `is` [ADD, SUB]) do
        s.result <== truncate sum

      when (instr.opcode `is` [SLT, SLTU]) do
        s.result <== zeroExtend less

      when (instr.opcode `is` [AND]) do
        s.result <== opr1 .&. opr2orImm

      when (instr.opcode `is` [OR]) do
        s.result <== opr1 .|. opr2orImm

      when (instr.opcode `is` [XOR]) do
        s.result <== opr1 .^. opr2orImm

      when (instr.opcode `is` [LUI]) do
        s.result <== opr2orImm

      -- Barrel shifter
      let shiftAmount = slice @4 @0 opr2orImm

      when (instr.opcode `is` [SLL]) do
        s.result <== opr1 .<<. shiftAmount

      when (instr.opcode `is` [SRL, SRA]) do
        let ext = instr.opcode `is` [SRA] ? (at @31 opr1, 0)
        let opr1Ext = ext # opr1
        s.result <== truncate (opr1Ext .>>>. shiftAmount)

      let branch =
            orList [
              instr.opcode `is` [BEQ] .&. equal
            , instr.opcode `is` [BNE] .&. inv equal
            , instr.opcode `is` [BLT, BLTU] .&. less
            , instr.opcode `is` [BGE, BGEU] .&. inv less
            ]

      when branch do
        s.pc <== s.pc.val + signExtend instr.off

      let pcNew = s.pc.val + opr2orImm
      when (instr.opcode `is` [AUIPC]) do
        s.result <== pcNew

      when (instr.opcode `is` [JAL]) do
        s.pc <== pcNew

      let plus = opr1 + opr2orImm
      when (instr.opcode `is` [JALR]) do
        s.pc <== truncateLSB plus # (0 :: Bit 1)

      when (instr.opcode `is` [JAL, JALR]) do
        s.result <== s.pc.val + 4

      when (instr.opcode `is` [ECALL]) do
        return ()

      when (instr.opcode `is` [EBREAK]) do
        return ()

      -- Memory fence
      when (instr.opcode `is` [FENCE]) do
        return ()

      -- Control/status registers
      when (instr.opcode `is` [CSRRW, CSRRS, CSRRC]) do
        -- Condition for reading CSR
        let doRead = instr.opcode `is` [CSRRW] ? (instr.rd.val .!=. 0, true)
        -- Read CSR
        x <- whenAction doRead do csrUnit.read instr.csr
        s.result <== x
        -- Condition for writing CSR
        let rs1 = instr.rs1.val
        let doWrite = instr.opcode `is` [CSRRS, CSRRC] ? (rs1 .!=. 0, true)
        -- Determine operand
        let operand = instr.csrI ? (zeroExtend rs1, opr1)
        -- Data to write for CSRRS/CSRRC
        let maskedData = fromBitList
              [ cond ? (instr.opcode `is` [CSRRS], old)
              | (old, cond) <- zip (toBitList x) (toBitList operand) ]
        -- Data to write
        let writeData = instr.opcode `is` [CSRRW] ? (operand, maskedData)
        -- Write CSR
        when doWrite do csrUnit.write instr.csr writeData

      -- Memory access
      when (instr.opcode `is` [LOAD, STORE]) do
        s.memReq <==
          MemReq {
            op = if instr.opcode `is` [LOAD] then memLoadOp else memStoreOp
          , addr = plus
          , payload = opr2
          , accessWidth = instr.accessWidth
          , isUnsigned = instr.isUnsigned
          }

-- Memory alignment helpers
-- ========================

-- Access width
type AccessWidth = Bit 2

-- | Byte, half-word, or word word access?
isByteAccess, isHalfAccess, isWordAccess :: AccessWidth -> Bit 1
isByteAccess = (.==. 0b00)
isHalfAccess = (.==. 0b01)
isWordAccess = (.==. 0b10)

-- Determine byte enables given access-width and address
genByteEnable :: AccessWidth -> Bit 32 -> Bit 4
genByteEnable w addr =
  select [
    isWordAccess w --> 0b1111
  , isHalfAccess w --> (a.==.2) # (a.==.2) # (a.==.0) # (a.==.0)
  , isByteAccess w --> (a.==.3) # (a.==.2) # (a.==.1) # (a.==.0)
  ]
  where a :: Bit 2 = truncate addr

-- Align write-data using access-width
writeAlign :: AccessWidth -> Bit 32 -> Bit 32
writeAlign w d =
  select [
    isWordAccess w --> b3 # b2 # b1 # b0
  , isHalfAccess w --> b1 # b0 # b1 # b0
  , isByteAccess w --> b0 # b0 # b0 # b0
  ]
  where
    b0 = slice @7 @0 d
    b1 = slice @15 @8 d
    b2 = slice @23 @16 d
    b3 = slice @31 @24 d

-- Determine result of load from memory response
loadMux :: Bit 32 -> Bit 2 -> AccessWidth -> Bit 1 -> Bit 32
loadMux respData a w isUnsigned =
    select [
      isWordAccess w --> b3 # b2 # b1 # b0
    , isHalfAccess w --> hExt # h
    , isByteAccess w --> bExt # b
    ]
  where
    b = select [
          a .==. 0 --> b0
        , a .==. 1 --> b1
        , a .==. 2 --> b2
        , a .==. 3 --> b3
        ]
    h = (at @1 a .==. 0) ? (b1 # b0, b3 # b2)
    bExt = isUnsigned ? (0, signExtend (at @7 b))
    hExt = isUnsigned ? (0, signExtend (at @15 h))
    b0 = slice @7 @0 respData
    b1 = slice @15 @8 respData
    b2 = slice @23 @16 respData
    b3 = slice @31 @24 respData

-- Tightly-coupled memories
-- ========================

-- Instruction tightly-coupled memory
type ITCM = Server (Bit XLen) (Bit XLen)

makeITCM :: Int -> String -> Module ITCM
makeITCM logSize initFile =
  -- Determine address width at type level
  liftNat logSize \(_ :: Proxy addrW) -> do

    -- State
    ram   :: RAM (Bit addrW) (Bit XLen) <- makeDualRAMInit initFile
    put   :: Wire (Bit XLen)            <- makeWire dontCare
    queue :: Queue (Bit 0)              <- makePipelineQueue 1

    always do
      if put.active
        then do
          queue.enq dontCare
          ram.load (truncateCast (upper put.val :: Bit (XLen-2)))
        else do
          ram.preserveOut

    return
      Server {
        reqs =
          Sink {
            canPut = queue.notFull
          , put    = \addr -> do put <== addr
          }
      , resps =
          Source { 
            canPeek = queue.canDeq
          , peek    = ram.out
          , consume = queue.deq
          }
      }

-- Data tightly-coupled memory
type DTCM = Server MemReq (Bit XLen)

makeDTCM :: Int -> String -> Module DTCM
makeDTCM logSize initFile =
  -- Determine address width at type level
  liftNat logSize \(_ :: Proxy addrW) -> do

    -- State
    ram   :: RAMBE addrW 4 <- makeRAMInitBE initFile
    put   :: Wire MemReq   <- makeWire dontCare
    queue :: Queue MemReq  <- makePipelineQueue 1

    always do
      if put.active
        then do
          let req = put.val
          let addr = truncateCast (upper req.addr :: Bit (XLen-2))
          let byteEn = genByteEnable req.accessWidth req.addr
          let writeVal = writeAlign req.accessWidth req.payload
          if req.op .==. memLoadOp
            then do
              ram.loadBE addr
              queue.enq req
            else do
              ram.storeBE addr byteEn writeVal
              ram.preserveOutBE
        else do
          ram.preserveOutBE

    return
      Server {
        reqs =
          Sink {
            canPut = queue.notFull
          , put    = \req -> do put <== req
          }
      , resps =
          Source { 
            canPeek = queue.canDeq
          , peek    = loadMux ram.outBE
                              (truncate queue.first.addr)
                              queue.first.accessWidth
                              queue.first.isUnsigned
          , consume = queue.deq
          }
      }

-- Control/status registers
-- ========================

-- Create a RISC-V CSR unit with the following CSRs:
--
-- UARTRead (0x800): Reading from this yields a byte from the UART in
-- bits [7:0], while bit 8 contains whether the byte is present or not.
--
-- UARTWrite (0x801): Reading from this yields a single bit denoting
-- whether the UART write buffer is not full. Writing to it
-- inserts a byte into the buffer.

makeCSRUnit :: Stream (Bit 8) -> Module (CSRUnit, Stream (Bit 8))
makeCSRUnit fromUART = do
  writeBuffer <- makeShiftQueue 1

  let csrUnit =
        CSRUnit {
          read = \csr -> do
            -- Destructive aspects
            when (csr .==. 0x800 .&&. fromUART.canPeek) do
              fromUART.consume
            -- Non-desctructive aspects
            let result =
                  select [
                    csr .==. 0x800 -->
                      (0 # fromUART.canPeek # fromUART.peek)
                  , csr .==. 0x801 --> zeroExtend writeBuffer.notFull
                  ]
            return result
        , write = \csr payload ->
            when (csr .==. 0x801 .&&. writeBuffer.notFull) do
              writeBuffer.enq (truncate payload)
        } 

  return (csrUnit, toStream writeBuffer)

-- RV32I microcontroller
-- =====================

makeMicrocontroller :: AvalonJTAGUARTIns -> Module AvalonJTAGUARTOuts
makeMicrocontroller avlUARTIns = mdo
  -- Use register forwarding?
  let useForwarding = False
  -- Use branch target prediction?
  let useBranchPred = False
  -- 16KiB each for instruction memory and data memory
  -- (We assume these are the same size to avoid explicit memory mapping)
  imem <- makeITCM 12 "imem.mif"
  dmem <- makeDTCM 12 "dmem.mif"
  -- Register memory
  rmem <- if useForwarding
            then makeForwardingRegMemRAM 2
            else makeRegMemRAM 2
  -- CSR unit
  (csrUnit, toUART) <- makeCSRUnit fromUART
  -- JTAG UART
  (fromUART, avlUARTOuts) <- makeJTAGUART toUART avlUARTIns
  -- Instruction set
  let iset = instrSet csrUnit
  -- Classic 5-stage pipeline
  s <- makeClassic
    PipelineParams {
      initPC         = 0
    , instrLen       = 4
    , imem           = imem
    , dmem           = dmem
    , instrSet       = iset
    , makeBranchPred = if useBranchPred
                         then makeBTBPredictor @8 4
                         else makeNaivePredictor 4
    , makeRegFile    = if useForwarding
                         then makeForwardingRegFile rmem iset
                         else makeBasicRegFile rmem iset
    }
  return avlUARTOuts

-- Main
-- ====

main :: IO ()
main = writeVerilogModule makeMicrocontroller "FiveAlive" "./"
