import Data.Maybe
import Control.Monad
import Data.Tuple
import Data.Char

data Instruction = IncPointer | DecPointer | IncVal | DecVal | ReadVal | PrintVal | StartLoop | EndLoop deriving Eq
type Program = [Instruction]
type Stack = [Program]

type Field = Int
type Stream = [Field]
type Memory = (Stream, Stream)

type State = (Stack, Memory, Stream, Stream)

type Error = String

-- The input memory is always assumed to be in a valid state

-- >
increment_pointer :: Memory -> Memory
increment_pointer (p, [n]) = (n:p, [0])
increment_pointer (p, x:xs) = (x:p, xs)

-- <
decrement_pointer :: Memory -> Memory
decrement_pointer ([], xs) = ([], 0:xs)
decrement_pointer m = (swap.increment_pointer.swap) m

-- .
set_location :: Field -> Memory -> Memory
set_location f (p, x:xs) = (p, f:xs)

-- ,
get_location :: Memory -> Field
get_location (p, x:xs) = x

-- +
increment_value :: Memory -> Memory
increment_value (p, 255:xs) = (p, 0:xs)
increment_value (p, x:xs) = (p, (x+1):xs)

-- -
decrement_value :: Memory -> Memory
decrement_value (p, 0:xs) = (p, 255:xs)
decrement_value (p, x:xs) = (p, (x-1):xs)

-- [
push_stack :: Stack -> Stack
push_stack (x:xs) = (x:x:xs)

-- ]
pop_stack :: Stack -> Stack
pop_stack [] = []
pop_stack s = tail s


skip_order :: (Int, Program) -> (Int, Program)
skip_order (n, []) = (n, [])
skip_order (n, prog)
  | head prog == StartLoop = (n+1, tail prog)
  | head prog == EndLoop   = (n-1, tail prog)
  | otherwise              = (n, tail prog)


skip_to_matching_bracket :: Program -> Program -- [.. -> ]..
skip_to_matching_bracket prog =
  if head prog /= StartLoop then
    prog -- invalid input, return input
  else
    EndLoop : (snd $ head $ dropWhile (\x -> (fst x) /= 0) $ tail $ iterate skip_order (0, prog))

  
map_head :: (a->a) -> [a] -> [a]
map_head f (x:xs) = (f x) : xs

advance_stack :: Stack -> Stack
advance_stack = map_head tail

step_program :: State -> State
step_program (stack, memory, input, output) =
  let
    instruction = head $ head stack
    new_stack = advance_stack stack
  in
    case instruction of IncVal     -> (new_stack, increment_value memory, input, output)
                        DecVal     -> (new_stack, decrement_value memory, input, output)
                        IncPointer -> (new_stack, increment_pointer memory, input, output)
                        DecPointer -> (new_stack, decrement_pointer memory, input, output)
                        ReadVal    -> (new_stack, set_location (head input) memory, tail input, output)
                        PrintVal   -> (new_stack, memory, input, output ++ [get_location memory])
                        StartLoop  ->
                          if get_location memory == 0 then
                            (map_head skip_to_matching_bracket stack, memory, input, output)
                          else
                            (advance_stack $ push_stack stack, memory, input, output)
                        EndLoop    ->
                          if get_location memory == 0 then
                            (advance_stack $ map_head skip_to_matching_bracket $ pop_stack stack, memory, input, output)
                          else
                            (pop_stack stack, memory, input, output)


program_done :: State -> Bool
program_done ([[]], _, _, _) = True
program_done _ = False

run_program :: Program -> Stream -> Either Error Stream
run_program program input =
  let
    maxlen = 100000
    initial_state = ([program], ([],[0]), input, [])
    states = iterate step_program initial_state
    not_done_states = takeWhile ( not.program_done ) $ take maxlen states
  in
    if length not_done_states == maxlen then
      Left "PROCESS TIME OUT. KILLED!!!"
    else
      let (_,_,_,output) = step_program $ last not_done_states
      in Right output



parseInstruction :: Char -> Maybe Instruction
parseInstruction c
  | c == '<'  = Just DecPointer
  | c == '>'  = Just IncPointer
  | c == '-'  = Just DecVal
  | c == '+'  = Just IncVal
  | c == '.'  = Just PrintVal
  | c == ','  = Just ReadVal
  | c == '['  = Just StartLoop
  | c == ']'  = Just EndLoop
  | otherwise = Nothing

parseProgram :: String -> Program -- invalid chars are dropped
parseProgram = catMaybes.(map parseInstruction)

execute_program :: String -> String -> String
execute_program program_string input =
  let
    program = parseProgram program_string
    input_stream = map ord input
    result = run_program program input_stream
  in
    case result of Right output -> map chr output
                   Left error -> error

main = do
  n_m <- getLine
  let nm_list = map read $ words n_m :: [Int]
  let n = head nm_list
  let m = last nm_list
  input <- take n <$> getLine
  program <- fmap concat $ forM [1..m] $ \_ -> getLine
  putStrLn $ execute_program program input
  
