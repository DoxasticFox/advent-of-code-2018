import Data.Bits
import System.IO.Unsafe


data RegisterIndex =
    R0 |
    R1 |
    R2 |
    R3 |
    R4 |
    R5

type RegisterValue = Int

type MachineState = (
    RegisterValue,
    RegisterValue,
    RegisterValue,
    RegisterValue,
    RegisterValue,
    RegisterValue
    )

data Machine = Machine {
    instructionPointerIndex :: Int,
    state :: MachineState
} deriving (Read, Show, Eq)

type Instruction = (Op, RegisterIndex, RegisterIndex, RegisterIndex)

intToRegisterIndex :: Int -> RegisterIndex
intToRegisterIndex 0 = R0
intToRegisterIndex 1 = R1
intToRegisterIndex 2 = R2
intToRegisterIndex 3 = R3
intToRegisterIndex 4 = R4
intToRegisterIndex 5 = R5

readRegister :: Machine -> RegisterIndex -> RegisterValue
readRegister machine index =
    readStateRegister (state machine) index

writeRegister :: Machine -> RegisterIndex -> RegisterValue -> Machine
writeRegister machine index value =
    machine {
        state = writeStateRegister (state machine) index value
    }

readStateRegister :: MachineState -> RegisterIndex -> RegisterValue
readStateRegister (r, _, _, _, _, _) R0 = r
readStateRegister (_, r, _, _, _, _) R1 = r
readStateRegister (_, _, r, _, _, _) R2 = r
readStateRegister (_, _, _, r, _, _) R3 = r
readStateRegister (_, _, _, _, r, _) R4 = r
readStateRegister (_, _, _, _, _, r) R5 = r

writeStateRegister :: MachineState -> RegisterIndex -> RegisterValue -> MachineState
writeStateRegister (_,  r2, r3, r4, r5, r6) R0 v = (v,  r2, r3, r4, r5, r6)
writeStateRegister (r1, _,  r3, r4, r5, r6) R1 v = (r1, v,  r3, r4, r5, r6)
writeStateRegister (r1, r2, _,  r4, r5, r6) R2 v = (r1, r2, v,  r4, r5, r6)
writeStateRegister (r1, r2, r3, _,  r5, r6) R3 v = (r1, r2, r3, v,  r5, r6)
writeStateRegister (r1, r2, r3, r4, _,  r6) R4 v = (r1, r2, r3, r4, v,  r6)
writeStateRegister (r1, r2, r3, r4, r5, _ ) R5 v = (r1, r2, r3, r4, r5, v )

open = lines . unsafePerformIO . readFile

left  l r = l
gt    l r = if l > r  then 1 else 0
eq    l r = if l == r then 1 else 0

applyrr
    :: Machine
    -> (RegisterValue -> RegisterValue -> RegisterValue)
    -> RegisterIndex
    -> RegisterIndex
    -> RegisterIndex
    -> Machine
applyrr machine func a b c =
    let
        aVal = readRegister machine a
        bVal = readRegister machine b
    in
        writeRegister machine c (func aVal bVal)

applyri
    :: Machine
    -> (RegisterValue -> RegisterValue -> RegisterValue)
    -> RegisterIndex
    -> RegisterValue
    -> RegisterIndex
    -> Machine
applyri machine func a bVal c =
    let
        aVal = readRegister machine a
    in
        writeRegister machine c (func aVal bVal)

applyir
    :: Machine
    -> (RegisterValue -> RegisterValue -> RegisterValue)
    -> RegisterValue
    -> RegisterIndex
    -> RegisterIndex
    -> Machine
applyir machine func aVal b c =
    let
        bVal = readRegister machine b
    in
        writeRegister machine c (func aVal bVal)

addr, mulr, banr, borr, setr
    :: Machine -> RegisterIndex -> RegisterIndex -> RegisterIndex -> Machine
addi, muli, bani, bori, seti
    :: Machine -> RegisterIndex -> RegisterValue -> RegisterIndex -> Machine

gtir, eqir
    :: Machine -> RegisterValue -> RegisterIndex -> RegisterIndex -> Machine
gtri, eqri
    :: Machine -> RegisterIndex -> RegisterValue -> RegisterIndex -> Machine
gtrr, eqrr
    :: Machine -> RegisterIndex -> RegisterIndex -> RegisterIndex -> Machine

addr machine a b c = applyrr machine (+)    a b c
mulr machine a b c = applyrr machine (*)    a b c
banr machine a b c = applyrr machine (.&.)  a b c
borr machine a b c = applyrr machine (.|.)  a b c
setr machine a b c = applyrr machine (left) a b c

addi machine a b c = applyri machine (+)    a b c
muli machine a b c = applyri machine (*)    a b c
bani machine a b c = applyri machine (.&.)  a b c
bori machine a b c = applyri machine (.|.)  a b c
seti machine a b c = applyri machine (left) a b c

gtir machine a b c = applyir machine gt     a b c
gtri machine a b c = applyri machine gt     a b c
gtrr machine a b c = applyrr machine gt     a b c

eqir machine a b c = applyir machine eq     a b c
eqri machine a b c = applyri machine eq     a b c
eqrr machine a b c = applyrr machine eq     a b c


parseInstructions :: [String] -> [Instruction]
parseInstructions [] = []
parseInstructions (s:ss) =
    [parseInstruction s] ++ parseInstructions ss

parseInstruction :: String -> Instruction
parseInstruction s =
    let
        op:a:b:c:[] = splitOn " " s
    in
        [parseOp op] ++ 


--    addr (add register) stores into register C the result of adding register A and register B.
--    addi (add immediate) stores into register C the result of adding register A and value B.
--
--Multiplication:
--
--    mulr (multiply register) stores into register C the result of multiplying register A and register B.
--    muli (multiply immediate) stores into register C the result of multiplying register A and value B.
--
--Bitwise AND:
--
--    banr (bitwise AND register) stores into register C the result of the bitwise AND of register A and register B.
--    bani (bitwise AND immediate) stores into register C the result of the bitwise AND of register A and value B.
--
--Bitwise OR:
--
--    borr (bitwise OR register) stores into register C the result of the bitwise OR of register A and register B.
--    bori (bitwise OR immediate) stores into register C the result of the bitwise OR of register A and value B.
--
--Assignment:
--
--    setr (set register) copies the contents of register A into register C. (Input B is ignored.)
--    seti (set immediate) stores value A into register C. (Input B is ignored.)
--
--Greater-than testing:
--
--    gtir (greater-than immediate/register) sets register C to 1 if value A is greater than register B. Otherwise, register C is set to 0.
--    gtri (greater-than register/immediate) sets register C to 1 if register A is greater than value B. Otherwise, register C is set to 0.
--    gtrr (greater-than register/register) sets register C to 1 if register A is greater than register B. Otherwise, register C is set to 0.
--
--Equality testing:
--
--    eqir (equal immediate/register) sets register C to 1 if value A is equal to register B. Otherwise, register C is set to 0.
--    eqri (equal register/immediate) sets register C to 1 if register A is equal to value B. Otherwise, register C is set to 0.
--    eqrr (equal register/register) sets register C to 1 if register A is equal to register B. Otherwise, register C is set to 0.

main = do
    print $ open "input"
