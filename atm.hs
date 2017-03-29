
import System.IO
import Data.Map(Map,fromList)
import qualified Data.Map as M
import Data.Maybe(fromMaybe)

--This defines a bunch of types. They're similar to C "structs", though also quite different...

--This defines a Tape in the ATM. It has a len and a tape_arr so that it can handle negative indices
data Tape = Tape {len :: Int, tape_arr :: [Symbol]} deriving (Eq)

--A configuration of the Turing machine, defined as a single State, a position, and a Tape
data Configuration = Configuration {config_state :: State, pos :: Int, tape :: Tape} deriving (Eq)

--A configuration Node, defined as a Configuration with potential configuration node children, each representing a sequential configuration from the current one
data ConfigNode = ConfigNode {configuration :: Configuration, children :: [ConfigNode]} deriving (Eq)

--The acceptance state of a given configuration... I should probably refactor it to a better name
data ConfigType = Error | Rejecting | Deciding | Accepting deriving (Show, Eq, Ord, Bounded)


--The states will be simply represented as strings
type State = [Char]

--Symbols can either be a character or a Blank
data Symbol = Symbol Char | Blank deriving (Eq, Ord)

--The TapeAlphabet will be a list of symbols.
type TapeAlphabet = [Symbol]

-- ... Left or Right... Not much explanation necessary
data LeftRight = L | R deriving (Show, Eq, Read)

-- Defines a single transition, meaning the next state for this configuration, the symbol to write over, and the direction to move the "head"
data Transition = Transition State Symbol LeftRight deriving Show

-- Defines the transition function /delta/ of an ATM. It maps a State and symbol to a list of transitions.
-- Function Domain is (Q x Gamma) -> P(Q x Gamma x {L|R})
type TransitionFunction = Map State (Map Symbol [Transition])

-- One of the 4 types each state can represent, defined in the g function
data StateType = And | Or | Accept | Reject deriving (Show, Eq, Read)
 --Error is not considered a valid state, it is only there for if I have broken code

-- The Alternating Turing Machine Data object, mirroring its formal definition
data ATM = ATM {_Q :: [State]
               , _Gamma :: TapeAlphabet
               , delta :: TransitionFunction
               , q0 :: State
               , g :: Map State StateType} deriving Show

{-  Here are a lot of Show defines. They tell us how to render different objects-}
instance Show Symbol where
  show symbol = [symbol_char symbol]

instance Show Tape where
  show (Tape len' tape_arr') = '[':(show len') ++ (']':(map symbol_char tape_arr'))

instance Show Configuration where
  show config = (config_state config) ++ ':':(map symbol_char a) ++ '>':(map symbol_char b)
                where (a,b) = splitAt ((pos config) + (len $ tape config)) (tape_arr $ tape config)

instance Show ConfigNode where
  show config_node = show_depth 0 config_node
    where show_depth :: Int -> ConfigNode -> [Char]
          show_depth depth (ConfigNode config children') = ((replicate depth '|') ++ '*':(show config)
                                                           ++ '\n':(concat $ map (show_depth (succ depth)) children'))

--Gets a starting configuration node for an ATM given an input tape
start_config_input :: ATM -> Tape -> ConfigNode
start_config_input atm tape' = (ConfigNode (Configuration (q0 atm) 0 tape') [])

--Gets a default starting configuration for an ATM with a blank tape
start_config :: ATM -> ConfigNode
start_config atm = start_config_input atm (Tape 0 [Blank])

--Turns a Symbol into a char
symbol_char :: Symbol -> Char
symbol_char (Symbol a) = a
symbol_char Blank = '_'

--Turns a char into a Symbol
char_symbol :: Char -> Symbol
char_symbol '_' = Blank
char_symbol a = Symbol a

--Modifies the tape at a given position to a new symbol. Sizes up Tapes to be proper length
change_tape :: Tape -> Int -> Symbol -> Tape
change_tape tape' pos' symbol
  | abs_pos > len' = change_tape (Tape (len' + 1) (Blank:arr ++ [Blank])) pos' symbol
  | otherwise =  Tape len' $ a ++ (symbol:b)
   where Tape len' arr = tape'
         (a,_:b) = splitAt (len' + pos') arr
         abs_pos = succ $ abs pos'

--What is the symbol at the current location?
current_symbol :: Configuration -> Symbol
current_symbol (Configuration _ pos' (Tape len' tape'))
  | (abs pos') > len' = Blank
  | otherwise = tape' !! (len' + pos')

--Gets all the transitions from the current configuration
get_transitions :: Configuration -> TransitionFunction -> [Transition]
get_transitions config transFunc = (Data.Maybe.fromMaybe []
                          $ maybe Nothing (M.lookup (current_symbol config))
                          $ M.lookup (config_state config) transFunc)

--Applies a transition to a configuration
apply_transition :: Configuration -> Transition -> Configuration
apply_transition config trans = Configuration state (new_pos pos' leftright) (change_tape tape' pos' symbol)
  where new_pos ps L = pred ps
        new_pos ps R = succ ps
        (Configuration _ pos' tape') = config
        (Transition state symbol leftright) = trans

--Gets a list of ConfigNode children based on a transition function and a specific configuration
next_states ::TransitionFunction -> Configuration -> [ConfigNode]
next_states transFunc config =
  [(ConfigNode (apply_transition config trans) []) | trans <- get_transitions config transFunc]

--Runs the next step of this node
step_node :: ATM -> ConfigNode -> ConfigNode
step_node atm (ConfigNode config []) = (ConfigNode config (next_states (delta atm) config))
step_node atm (ConfigNode config children') = (ConfigNode config [(step_node atm child) | child <- children'])

--Determines the acceptance state (ConfigType) of the ConfigNode
get_config_type :: ATM -> ConfigNode -> ConfigType
get_config_type atm node
  | node_state_type == Accept = Accepting
  | node_state_type == Reject = Rejecting
  | node_state_type == Or && children' == [] = Deciding
  | node_state_type == Or = foldl max Rejecting (map (get_config_type atm) children')
  | node_state_type == And && children' == [] = Deciding
  | node_state_type == And = foldl min Accepting (map (get_config_type atm) children')
  | otherwise = Error
  where (ConfigNode config children') = node
        Just node_state_type = M.lookup (config_state config) (g atm)

--Creates a tape from a string
string_tape :: String -> Tape
string_tape string = let tape_len = length string in
  Tape tape_len ((replicate tape_len Blank) ++ (map char_symbol string) ++ [Blank])

--Defines individual commands in ATM files
update_atm :: ATM -> [String] -> ATM
update_atm atm ("init":str:_) = atm {q0=str}
update_atm atm ("states":star) = atm {_Q = ((_Q atm) ++ star), q0 = head star}
update_atm atm ("gamma":str:_) = atm {_Gamma = Blank:(map char_symbol str)}
update_atm atm ("g":state_:type_:_) = atm {g = M.insert state_ (read type_ :: StateType) (g atm)}
update_atm atm ("delta":src_st:[src_sym]:dst_st:[dst_sym]:lr:_) = let
  src_symbol = (char_symbol src_sym)
  old_state_transitions = M.lookup src_st (delta atm) :: Maybe (Map Symbol [Transition])
  old_transitions = fromMaybe [] $ maybe Nothing (M.lookup (char_symbol src_sym)) old_state_transitions :: [Transition]
  new_state_transitions = (Transition dst_st (char_symbol dst_sym) (read lr :: LeftRight)):old_transitions :: [Transition]
  newDelta = M.insert src_st (maybe
                              (fromList [(src_symbol,new_state_transitions)])
                              (M.insert src_symbol new_state_transitions)
                              old_state_transitions) (delta atm) ::TransitionFunction
  in atm {delta= newDelta}--More complicated
update_atm atm _ = atm

--Reads an ATM from a file
read_atm :: String -> IO ATM
read_atm x = do
  atm_handle <- openFile x ReadMode
  foldl (\atm-> update_atm atm . words) (ATM [] [] (fromList []) [] (fromList [])) <$> lines <$> hGetContents atm_handle

--Displays the acceptance state information of a confignode as a tree
accept_tree :: ATM -> ConfigNode -> String
accept_tree atm config_node = show_depth 0 config_node
    where show_depth :: Int -> ConfigNode -> [Char]
          show_depth depth configNode = ((replicate depth '|') ++ '-':(show node_state_type) ++ ':':(show (get_config_type atm configNode))
                                                           ++ '\n':(concat $ map (show_depth (succ depth)) (children configNode)))
            where Just node_state_type = M.lookup (config_state $ configuration configNode) (g atm)

--Displays the typically display of a ConfigNode /with/ the acceptance state information
whole_tree :: ATM -> ConfigNode -> String
whole_tree atm config_node = foldl (\str (x,y) -> str ++ x ++ "\n" ++ y ++ "\n") ""
                             $ zip (words $ show config_node)
                             (words $ accept_tree atm config_node)

-- Runs until it makes a decision... Hopefully it reaches one!
try_to_decide :: ATM -> ConfigNode -> String
try_to_decide = decide_step 1
  where
    decide_step :: Integer -> ATM -> ConfigNode -> String
    decide_step count atm confignode
          | config_type /= Deciding =  (show count) ++ '-':'>':(show config_type) ++ '\n':(whole_tree atm confignode)
          | otherwise = decide_step (succ count) atm (step_node atm confignode)
          where config_type = get_config_type atm confignode


--Convenience method, loads an ATM and then creates the ConfigNode root for it with a tape that looks like the second string
-- I.E. (atm,root) <- atm_and_root "sexy_atm.txt" "101010"
atm_and_root :: String -> String -> IO (ATM,ConfigNode)
atm_and_root atm_str cnfg_str = do
  atm <- read_atm atm_str
  let config_node = start_config_input atm (string_tape cnfg_str)
  return (atm,config_node)

--Convenience method, just does "try_to_decide", but then puts out the output instead of returning the String
put_decide :: ATM -> ConfigNode -> IO ()
put_decide x y = putStrLn $ try_to_decide x y

main :: IO String
main = {-let transition = Transition "q1" (Symbol '0') L
           transition' = Transition "q2" (Symbol '1') L
           transition'' = Transition "q3" (Symbol '0') R
           transFunc = (fromList [
                           ("q0", (fromList [(Blank, [transition])])),
                           ("q1", (fromList [(Blank, [transition',transition''])]))
                                 ])
           atm = ATM ["q0","q1","q2","q3"] [Symbol '0', Symbol '1', Blank] transFunc "q0" (fromList [("q0",Or),
                                                                                                     ("q1",And),
                                                                                                     ("q2",Accept),
                                                                                                     ("q3",Reject)])
           configNode = start_config atm
           nextStep = step_node atm configNode
      in -} do
{-  putStrLn $ show atm
  putStrLn "1"
  putStrLn $ show $ configNode
  putStrLn "2"
  putStrLn $ show nextStep
  putStrLn "3"
  putStrLn $ show (step_node atm nextStep)-}
  {-
  my_atm <- read_atm "base_atm.txt"
  putStrLn "my atm"
  let k = step_node my_atm (startConfig my_atm)
  let k2 = step_node my_atm k
  putStrLn $ show $ k2
  putStrLn $ accept_tree my_atm k2
  putStrLn $ whole_tree my_atm k2
  putStrLn $ try_to_decide my_atm (startConfig my_atm)
  putStrLn "Results from 2"
  return $ show $ get_config_type atm nextStep -}
  sexy_atm <- read_atm "sexy_atm.txt"
  putStrLn "What string do YOU think is sexy?"
  input <- getLine
  let results = try_to_decide sexy_atm (start_config_input sexy_atm (string_tape input))
  putStrLn results
  return "-"
  
