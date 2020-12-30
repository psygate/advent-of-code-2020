{-# LANGUAGE PartialTypeSignatures #-}

import Data.List.Split hiding (startsWith, endsWith)
import Data.List hiding (union)
import Data.String
import Control.Applicative
import Data.Char
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
import Control.Exception
import Debug.Trace

data Instruction = ACC Int | JMP Int | NOP Int deriving (Eq)
data AbortReason = LOOP | EOI deriving (Eq, Show)
type State = (Int, Int)

instance (Show Instruction) where
  show (ACC value) = ("acc " ++ (show value))
  show (JMP value) = ("jmp " ++ (show value))
  show (NOP value) = ("nop " ++ (show value))


newState :: State
newState = (0, 0)


getInstructionPointerValue :: State -> Int
getInstructionPointerValue (instrptr, _) = instrptr


getAccumulatorValue :: State -> Int
getAccumulatorValue (_, accvalue) = accvalue


setInstructionPointerValue :: State -> Int -> State
setInstructionPointerValue (instrptr, accvalue) update = (update, accvalue)


setAccumulatorValue :: State -> Int -> State
setAccumulatorValue (instrptr, accvalue) update = (instrptr, update)


addAccumulatorValue :: State -> Int -> State
addAccumulatorValue state@(instrptr, accvalue) update =  setAccumulatorValue state (accvalue + update)


addInstructionPointerValue :: State -> Int -> State
addInstructionPointerValue state@(instrptr, accvalue) update =  setInstructionPointerValue state (instrptr + update)


executeInstruction :: State -> Instruction -> State
executeInstruction state (ACC value) = addInstructionPointerValue (addAccumulatorValue state value) 1
executeInstruction state (NOP value) = addInstructionPointerValue state 1
executeInstruction state (JMP value) = addInstructionPointerValue state value


operandToInt :: String -> Int
operandToInt value
  | head value == '-' = - (read (drop 1 value))
  | head value == '+' = read (drop 1 value)
  | otherwise = read value


readInstruction :: String -> Instruction
readInstruction value
  | instr == "acc" = ACC intoperand
  | instr == "jmp" = JMP intoperand
  | instr == "nop" = NOP intoperand
  where (instr:operand:[]) = splitOn " " value
        intoperand = operandToInt operand


incrementInstructionCounter :: [(Int, Instruction)] -> Int -> [(Int, Instruction)]
incrementInstructionCounter ((count, instruction):xs) 0 = (count + 1, instruction):xs
incrementInstructionCounter (x:xs) value = x:(incrementInstructionCounter xs (value - 1))


executeInstructionsOnce' :: [(Int, Instruction)] -> State -> (State, AbortReason)
executeInstructionsOnce' instructions state
  | instructionPtr == length instructions = (state, EOI)
  | nextInstructionNeverExecuted = executeInstructionsOnce' updatedInstructions updatedState
  | not nextInstructionNeverExecuted = (state, LOOP)
  where instructionPtr = getInstructionPointerValue state
        (executed, currentInstruction) = instructions !! instructionPtr
        nextInstructionNeverExecuted = executed == 0
        updatedInstructions = incrementInstructionCounter instructions instructionPtr
        updatedState = executeInstruction state currentInstruction


executeInstructionsOnce :: [Instruction] -> (State, AbortReason)
executeInstructionsOnce instructions = executeInstructionsOnce' (map (\x -> (0, x)) instructions) newState


isNOP :: Instruction -> Bool
isNOP (NOP _) = True
isNOP _ = False


isACC :: Instruction -> Bool
isACC (ACC _) = True
isACC_ = False


isJMP :: Instruction -> Bool
isJMP (JMP _) = True
isJMP _ = False


flipJMPNOP :: [Instruction] -> Int -> [Instruction]
flipJMPNOP [] _ = []
flipJMPNOP ((JMP value):xs) 0 = (NOP value):xs
flipJMPNOP ((NOP value):xs) 0 = (JMP value):xs
flipJMPNOP ((JMP value):xs) idx = (JMP value):(flipJMPNOP xs (idx - 1))
flipJMPNOP ((NOP value):xs) idx = (NOP value):(flipJMPNOP xs (idx - 1))
flipJMPNOP (instr:xs) idx = instr:(flipJMPNOP xs (idx - 1))


diff :: (Eq a) => [a] -> [a] -> [(Int, a, a)]
diff a b = [(idx, lvalue, rvalue) | (idx, (lvalue, rvalue)) <- zip [0..] (zip a b), lvalue /= rvalue]


part1 :: String -> IO ()
part1 inputname = do
        rawinput <- readFile inputname
        let splitLines = lines rawinput
        let instructions = map readInstruction splitLines :: [Instruction]
        let (stateOutput, termReason) = executeInstructionsOnce instructions
        print $ assert (termReason == LOOP) ("Part1: " ++ (show $ getAccumulatorValue $ stateOutput))


part2 :: String -> IO ()
part2 inputname = do
        rawinput <- readFile inputname
        let splitLines = lines rawinput
        let instructions = map readInstruction splitLines :: [Instruction]

--        let replaceInstructionSize = length $ filter (\x -> isNOP x || isJMP x) instructions
        let programs = filter ((/=) instructions) $ map (flipJMPNOP instructions) [0..(length instructions) - 1]
        let executedStates = map executeInstructionsOnce programs
        let validStates = filter (\(state, endstate) -> endstate == EOI) executedStates
        let ((selectedState, termReason):[]) = validStates

        print $ assert (termReason == EOI) ("Part2: " ++ (show $ getAccumulatorValue $ selectedState))

        return ()

tests :: String -> IO ()
tests inputname = do
        rawinput <- readFile inputname
        let splitLines = lines rawinput
        let instructions = map readInstruction splitLines :: [Instruction]

        let simpleSet = [(0, ACC 1), (0, ACC 1), (0, ACC 1)]
        let simpleSet1 = [(1, ACC 1), (0, ACC 1), (0, ACC 1)]
        let simpleSet2 = [(0, ACC 1), (1, ACC 1), (0, ACC 1)]
        let simpleSet3 = [(0, ACC 1), (0, ACC 1), (1, ACC 1)]

        let simpleSet4 = [JMP 0, NOP 1, JMP 2]
        let simpleSet5 = [NOP 0, JMP 1, NOP 2]


        print $ assert (getInstructionPointerValue (0, 0) == 0) "[+]"
        print $ assert (getInstructionPointerValue (1, 0) == 1) "[+]"

        print $ assert (getAccumulatorValue (0, 0) == 0) "[+]"
        print $ assert (getAccumulatorValue (0, 1) == 1) "[+]"

        print $ assert (incrementInstructionCounter simpleSet 0 == simpleSet1) "[+]"
        print $ assert (incrementInstructionCounter simpleSet 1 == simpleSet2) "[+]"
        print $ assert (incrementInstructionCounter simpleSet 2 == simpleSet3) "[+]"

        print $ assert (executeInstructionsOnce (map (\(_, ins) -> ins) simpleSet) == ((3, 3), EOI)) "[+]"
        print $ assert (executeInstructionsOnce instructions == ((1, 5), LOOP)) "[+]"

        print $ assert (flipJMPNOP simpleSet4 0 == [NOP 0, NOP 1, JMP 2]) "[+]"
        print $ assert (flipJMPNOP simpleSet4 1 == [JMP 0, JMP 1, JMP 2]) "[+]"
        print $ assert (flipJMPNOP simpleSet4 2 == [JMP 0, NOP 1, NOP 2]) "[+]"

        print $ assert (flipJMPNOP (flipJMPNOP simpleSet4 0) 0 == simpleSet4) "[+]"
        print $ assert (flipJMPNOP (flipJMPNOP simpleSet4 1) 1 == simpleSet4) "[+]"
        print $ assert (flipJMPNOP (flipJMPNOP simpleSet4 2) 2 == simpleSet4) "[+]"

        return ()


main :: IO ()
main = do
  tests "test_input.txt"
  part1 "input.txt"
  part2 "input.txt"