{-|
The Translator provides a way to convert the specified virtual machine to assembly.
-}
module Hack.Translator
       ( Command(..)
       , Segment(..)
       , translate
       , parseVM
       ) where

import Data.Char (toLower, isSpace)
import Data.Either (partitionEithers)
import Data.List (elemIndices, groupBy, tails)
import Data.Maybe (catMaybes)
import Text.Read (readMaybe)

import Hack.Assembler


-- |References a certain segment of VM 'memory'
data Segment
  = Argument -- ^stores the function's arguments
  | Local -- ^stores the function's local variables
  | Static -- ^stores static variables shared by all functions in the same .vm file
  | Constant -- ^pseudo-segment that holds al the constants in the range 0...32767
  | This -- ^general-purpose segments can be made to correspond to different areas in the heap
  | That -- ^serves various programming needs
  | Pointer -- ^a two-entry segment that holds the base addresses of the this and that segements
  | Temp -- ^fixed eight-entry segment that holds temporary variables for general use
  deriving (Eq)
instance Show Segment where
  show Argument = "argument"
  show Local = "local"
  show Static = "static"
  show Constant = "constant"
  show This = "this"
  show That = "that"
  show Pointer = "pointer"
  show Temp = "temp"
instance Read Segment where
  readsPrec _ string
    | token == "argument" = [(Argument,remainder)]
    | token == "local"    = [(Local,remainder)]
    | token == "static"   = [(Static,remainder)]
    | token == "constant" = [(Constant,remainder)]
    | token == "this"     = [(This,remainder)]
    | token == "that"     = [(That,remainder)]
    | token == "pointer"  = [(Pointer,remainder)]
    | token == "temp"     = [(Temp,remainder)]
    | otherwise = []
    where
      token = map toLower $ takeWhile (not . isSpace) string
      remainder = dropWhile (not . isSpace) string


-- |All of the VM command types
data Command
  = Add -- ^Pops the top 2 values (STACK,x,y) from the stack and pushes their addition (x+y)
  | Sub -- ^Pops the top 2 values (STACK,x,y) from the stack and pushes their subtraction (x-y)
  | Neg -- ^Arithmetic negation of the top value of the stack
  | Eq -- ^Pops the top 2 values (STACK,x,y) from the stack and pushes -1 if equal else 0
  | Gt -- ^Pops the top 2 values (STACK,x,y) from the stack and pushes -1 if x > y else 0
  | Lt -- ^Pops the top 2 values (STACK,x,y) from the stack and pushes -1 if x < y else 0
  | And -- ^Pops the top 2 values (STACK,x,y) from the stack and pushes their bitwise AND
  | Or -- ^Pops the top 2 values (STACK,x,y) from the stack and pushes their bitwise OR
  | Not -- ^Bitwise NOT of the top value of the stack
  | Push Segment Int -- ^Pushes a value from the top of the stack into the specified memory area
  | Pop Segment Int -- ^Pops a value from the specified memory area to the top of the stack
  | Label String -- ^Declares a function-local label
  | Goto String -- ^Jumps to a label defined in the same function
  | IfGoto String -- ^Jumps if the top of the stack is not 0
  | Function String Int -- ^Defines a named function that has the specified number of local variables
  | Call String Int -- ^Calls the named function with the specified number of arguments already on the stack
  | Return -- ^Transfers flow back to the calling function
  deriving (Eq)
instance Show Command where
  show Add = "add"
  show Sub = "sub"
  show Neg = "neg"
  show Eq = "eq"
  show Gt = "gt"
  show Lt = "lt"
  show And = "and"
  show Or = "or"
  show Not = "not"
  show Return = "return"
  show (Label s) = "label " ++ s
  show (Goto s) = "goto " ++ s
  show (IfGoto s) = "if-goto " ++ s
  show (Push s i) = "push " ++ show s ++ " " ++ show i
  show (Pop s i) = "pop " ++ show s ++ " " ++ show i
  show (Function s i) = "function " ++ show s ++ " " ++ show i
  show (Call s i) = "call " ++ show s ++ " " ++ show i
instance Read Command where
  readsPrec _ string
    | null tokens = []
    | token1 == "add" = [(Add, tail1)]
    | token1 == "sub" = [(Sub, tail1)]
    | token1 == "neg" = [(Neg, tail1)]
    | token1 == "eq" = [(Eq, tail1)]
    | token1 == "gt" = [(Gt, tail1)]
    | token1 == "lt" = [(Lt, tail1)]
    | token1 == "and" = [(And, tail1)]
    | token1 == "or" = [(Or, tail1)]
    | token1 == "not" = [(Not, tail1)]
    | token1 == "return" = [(Return, tail1)]
    | length tokens < 2 = []
    | token1 == "label" = [(Label label2, tail2)]
    | token1 == "goto" = [(Goto label2, tail2)]
    | token1 == "if-goto" = [(IfGoto label2, tail2)]
    | length tokens < 3 = []
    | token1 == "push" = [(Push (read token2) (read token3), tail3)]
    | token1 == "pop" = [(Pop (read token2) (read token3), tail3)]
    | token1 == "function" = [(Function label2 (read token3), tail3)]
    | token1 == "call" = [(Call label2 (read token3), tail3)]
    | otherwise = []
    where
      tokens = words string
      token1 = map toLower $ head (words string)
      tail1 = dropWhile (not . isSpace) $ dropWhile isSpace string
      label2 = head (words tail1)
      token2 = map toLower label2
      tail2 = dropWhile (not . isSpace) $ dropWhile isSpace tail1
      token3 = map toLower $ head (words tail2)
      tail3 = dropWhile (not . isSpace) $ dropWhile isSpace tail2


-- COMMAND HELPERS
pushRoutine :: [Instruction]
-- Pushes register D onto the top of the stack and increments the SP
-- A will contain the address of the entry after the top of the stack (top+1)
pushRoutine = [Address (Symbol "SP") -- load the Stack pointer into A
              ,Calculate LoadM (Just StoreA) Nothing -- load the Stack address into A
              ,Calculate LoadD (Just StoreM) Nothing -- store the value in D onto the top of the Stack
              ,Address (Symbol "SP") -- load the Stack pointer into A
              ,Calculate IncM (Just StoreM) Nothing -- increment the SP and store it
              ]
popRoutine :: [Instruction]
-- Decrements the SP and pops the top of the stack into register D
-- A will contain the address of the entry after the top of the stack (top+1)
popRoutine = [Address (Symbol "SP") -- load the Stack pointer into A
             ,Calculate DecM (Just StoreAM) Nothing -- decrement the SP and store it (into A also)
             ,Calculate LoadM (Just StoreD) Nothing -- load the value at the top of the stack into D
             ]

pushSymRoutine :: String -> Int -> Either String [Instruction]
pushSymRoutine sym index
  | index < 0 = Left "Negative index specified"
  | index == 0 = Right $
                 [Address (Symbol sym) -- load the <Symbol> pointer into A
                 ,Calculate LoadM (Just StoreA) Nothing -- load the <Symbol> address into A
                 ,Calculate LoadM (Just StoreD) Nothing -- load the <Symbol> value into D
                 ] ++ pushRoutine
  | index == 0 = Right $
                 [Address (Symbol sym) -- load the <Symbol> pointer into A
                 ,Calculate IncM (Just StoreA) Nothing -- load the <Symbol> address into A
                 ,Calculate LoadM (Just StoreD) Nothing -- load the <Symbol> value into D
                 ] ++ pushRoutine
  | otherwise = Right $
                [Address (Symbol sym) -- load the <Symbol> pointer into A
                ,Calculate LoadM (Just StoreD) Nothing -- load the <Symbol> address into A
                ,Address (Literal index)
                ,Calculate AddDA (Just StoreA) Nothing -- calculate the <Symbol> address offset and store it into A
                ,Calculate LoadM (Just StoreD) Nothing -- load the <Symbol> value into D
                ] ++ pushRoutine

popSymRoutine :: String -> Int -> Either String [Instruction]
popSymRoutine sym index
  | index < 0 = Left "Negative index specified"
  | index == 0 = Right $
                 popRoutine ++
                 [Address (Symbol sym) -- load the <Symbol> pointer into A
                 ,Calculate LoadM (Just StoreA) Nothing -- load the <Symbol> address into A
                 ,Calculate LoadD (Just StoreM) Nothing -- store the popped value into the <Symbol> value
                 ]
  | index == 1 = Right $
                 popRoutine ++
                 [Address (Symbol sym) -- load the <Symbol> pointer into A
                 ,Calculate IncM (Just StoreA) Nothing -- load the <Symbol> address into A
                 ,Calculate LoadD (Just StoreM) Nothing -- store the popped value into the <Symbol> value
                 ]
  | otherwise = Right $
                [Address (Symbol sym) -- load the <Symbol> pointer into A
                ,Calculate LoadM (Just StoreD) Nothing -- load the <Symbol> address into D
                ,Address (Literal index)
                ,Calculate AddDA (Just StoreD) Nothing -- calculate the <Symbol> address offset
                ,Address (Symbol "R13")
                ,Calculate LoadD (Just StoreM) Nothing -- store the <Symbol> address into R13
                ] ++ popRoutine ++
                [Address (Symbol "R13")
                ,Calculate LoadM (Just StoreA) Nothing -- load the <Symbol> address from R13 into A
                ,Calculate LoadD (Just StoreM) Nothing -- store the popped value into the <Symbol> value
                ]

command2asm :: String -> Integer -> String -> Command -> Either String [Instruction]

-- ARITHMETIC COMMANDS

command2asm _ _ _ Add = Right $
                        popRoutine ++
                        [Calculate DecA (Just StoreA) Nothing -- store the address of the top element of the stack in A
                        ,Calculate AddDM (Just StoreM) Nothing -- add the previous Stack top value to the current and store
                        ]

command2asm _ _ _ Sub = Right $
                        popRoutine ++
                        [Calculate DecA (Just StoreA) Nothing -- store the address of the top element of the stack in A
                        ,Calculate SubMD (Just StoreM) Nothing -- subtract the previous Stack top value from the current and store
                        ]

command2asm _ _ _ Neg = Right
                        [Address (Symbol "SP") -- load the Stack pointer into A
                        ,Calculate DecM (Just StoreA) Nothing -- load the Stack address - 1 into A
                        ,Calculate NegM (Just StoreM) Nothing -- negate the Stack top value and store
                        ]

command2asm file lnum func Eq = Right
                                [Address (Symbol ("$EQLBL" ++ uname))
                                ,Calculate LoadA (Just StoreD) Nothing
                                ,Address (Symbol "R15")
                                ,Calculate LoadD (Just StoreM) Nothing -- store the custom address in R15
                                ,Address (Symbol "$STACKEQ")
                                ,Calculate Load0 Nothing (Just Jump) -- jump to the $STACKEQ routine
                                ,Marker ("$EQLBL" ++ uname) -- STACKEQ should jump back here when finished
                                ]
  where uname = file ++ show lnum ++ func -- create a custom address for returning from the subroutine

command2asm file lnum func Gt = Right
                                [Address (Symbol ("$GTLBL" ++ uname))
                                ,Calculate LoadA (Just StoreD) Nothing
                                ,Address (Symbol "R15")
                                ,Calculate LoadD (Just StoreM) Nothing -- store the custom address in R15
                                ,Address (Symbol "$STACKGT")
                                ,Calculate Load0 Nothing (Just Jump) -- jump to the $STACKGT routine
                                ,Marker ("$GTLBL" ++ uname) -- STACKGT should jump back here when finished
                                ]
  where uname = file ++ show lnum ++ func -- create a custom address for returning from the subroutine

command2asm file lnum func Lt = Right
                                [Address (Symbol ("$LTLBL" ++ uname))
                                ,Calculate LoadA (Just StoreD) Nothing
                                ,Address (Symbol "R15")
                                ,Calculate LoadD (Just StoreM) Nothing -- store the custom address in R15
                                ,Address (Symbol "$STACKLT")
                                ,Calculate Load0 Nothing (Just Jump) -- jump to the $STACKLT routine
                                ,Marker ("$LTLBL" ++ uname) -- STACKLT should jump back here when finished
                                ]
  where uname = file ++ show lnum ++ func -- create a custom address for returning from the subroutine

command2asm _ _ _ And = Right $
                        popRoutine ++
                        [Calculate DecA (Just StoreA) Nothing -- store the address of the top element of the stack in A
                        ,Calculate AndDM (Just StoreM) Nothing -- AND the previous Stack top value from the current and store
                        ]

command2asm _ _ _ Or = Right $
                       popRoutine ++
                       [Calculate DecA (Just StoreA) Nothing -- store the address of the top element of the stack in A
                       ,Calculate OrDM (Just StoreM) Nothing -- OR the previous Stack top value from the current and store
                       ]

command2asm _ _ _ Not = Right $
                        [Address (Symbol "SP") -- load the Stack pointer into A
                        ,Calculate DecM (Just StoreA) Nothing -- load the Stack address - 1 into A
                        ,Calculate InvM (Just StoreM) Nothing -- INV the Stack top value and store
                        ]

-- MEMORY ACCESS COMMANDS

-- MEMORY ACCESS COMMANDS - ARGUMENT

command2asm _ _ _ (Push Argument index) = pushSymRoutine "ARG" index

command2asm _ _ _ (Pop Argument index) = popSymRoutine "ARG" index

-- MEMORY ACCESS COMMANDS - LOCAL

command2asm _ _ _ (Push Local index) = pushSymRoutine "LCL" index

command2asm _ _ _ (Pop Local index) = popSymRoutine "LCL" index

-- MEMORY ACCESS COMMANDS - STATIC

command2asm file _ _ (Push Static index)
  | index < 0 = Left "Negative index specified"
  | otherwise = Right $
                [Address (Symbol (file ++ "." ++ show index)) -- load the Static address into A
                ,Calculate LoadM (Just StoreD) Nothing -- load the Static value into D
                ] ++ pushRoutine

command2asm file _ _ (Pop Static index)
  | index < 0 = Left "Negative index specified"
  | otherwise = Right $
                popRoutine ++
                [Address (Symbol (file ++ "." ++ show index)) -- load the Static address into A
                ,Calculate LoadD (Just StoreM) Nothing -- store the popped value
                ]

-- MEMORY ACCESS COMMANDS - CONSTANT

command2asm _ _ _ (Push Constant value)
  | value < 0 = Left $ "Constants cannot be negative (" ++ show value ++ " not allowed)"
  | value > 32767 = Left $ "Constants cannot exceed 32767 (" ++ show value ++ " not allowed)"
  | otherwise = Right $
                [Address (Literal value)
                ,Calculate LoadA (Just StoreD) Nothing -- store the constant into D
                ] ++ pushRoutine

command2asm _ _ _ (Pop Constant _) = Left "Cannot pop to the constant segment (that literally makes no sense)"

-- MEMORY ACCESS COMMANDS - THIS

command2asm _ _ _ (Push This index) = pushSymRoutine "THIS" index

command2asm _ _ _ (Pop This index) = popSymRoutine "THIS" index

-- MEMORY ACCESS COMMANDS - THAT

command2asm _ _ _ (Push That index) = pushSymRoutine "THAT" index

command2asm _ _ _ (Pop That index) = popSymRoutine "THAT" index

-- MEMORY ACCESS COMMANDS - POINTER

command2asm _ _ _ (Push Pointer 0) = Right $
                                     [Address (Symbol "THIS")
                                     ,Calculate LoadM (Just StoreD) Nothing -- load the THIS value into D
                                     ] ++ pushRoutine

command2asm _ _ _ (Pop Pointer 0) = Right $
                                    popRoutine ++
                                    [Address (Symbol "THIS")
                                    ,Calculate LoadD (Just StoreM) Nothing -- store the popped value into THIS
                                    ]

command2asm _ _ _ (Push Pointer 1) = Right $
                                     [Address (Symbol "THAT")
                                     ,Calculate LoadM (Just StoreD) Nothing -- load the THAT value into D
                                     ] ++ pushRoutine

command2asm _ _ _ (Pop Pointer 1) = Right $
                                    popRoutine ++
                                    [Address (Symbol "THAT")
                                    ,Calculate LoadD (Just StoreM) Nothing -- store the popped value into THAT
                                    ]

command2asm _ _ _ (Push Pointer _) = Left "Pointer Location Out Of Bounds (must be 0 (THIS) or 1 (THAT))"

command2asm _ _ _ (Pop Pointer _) = Left "Pointer Location Out Of Bounds (must be 0 (THIS) or 1 (THAT))"

-- MEMORY ACCESS COMMANDS - TEMP

command2asm _ _ _ (Push Temp index)
  | index < 0 = Left "Negative Temp index specified (must be 0-7)"
  | index > 7 = Left "Temp location out of bounds (must be 0-7)"
  | otherwise = Right $
                [Address (Symbol ('R' : show (5 + index))) -- load the Tempnth address into A
                ,Calculate LoadM (Just StoreD) Nothing -- load the Tempnth value into D
                ] ++ pushRoutine

command2asm _ _ _ (Pop Temp index)
  | index < 0 = Left "Negative Temp index specified (must be 0-7)"
  | index > 7 = Left "Temp location out of bounds (must be 0-7)"
  | otherwise = Right $
                popRoutine ++
                [Address (Symbol ('R' : show (5 + index))) -- load the Tempnth address into A
                ,Calculate LoadD (Just StoreM) Nothing -- store the popped value into the Tempth value
                ]

-- PROGRAM FLOW COMMANDS

command2asm _ _ func (Label label)
  | head label `elem` "0123456789" = Left $ "Invalid Label Format ('" ++ label ++ "' cannot start with a digit)"
  | not (all (`elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_.:0123456789") label) = Left $ "Invalid Label Format ('" ++ label ++ "' contains invalid character(s)"
  | otherwise = Right [Marker (func ++ "$" ++ label)] -- create the label

command2asm _ _ func (Goto label)
  | head label `elem` "0123456789" = Left $ "Invalid Label Format ('" ++ label ++ "' cannot start with a digit)"
  | not (all (`elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_.:0123456789") label) = Left $ "Invalid Label Format ('" ++ label ++ "' contains invalid character(s)"
  | otherwise = Right
                [Address (Symbol (func ++ "$" ++ label))
                ,Calculate Load0 Nothing (Just Jump) -- jump to the label
                ]

command2asm _ _ func (IfGoto label)
  | head label `elem` "0123456789" = Left $ "Invalid Label Format ('" ++ label ++ "' cannot start with a digit)"
  | not (all (`elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_.:0123456789") label) = Left $ "Invalid Label Format ('" ++ label ++ "' contains invalid character(s)"
  | otherwise = Right $
                popRoutine ++
                [Address (Symbol (func ++ "$" ++ label))
                ,Calculate LoadD Nothing (Just JumpNE) -- jump to the label if D is not 0
                ]

-- FUNCTION CALLING COMMANDS

command2asm _ _ _ (Function name argc)
  | head name `elem` "0123456789" = Left $ "Invalid Name Format ('" ++ name ++ "' cannot start with a digit)"
  | not (all (`elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_.:0123456789") name) = Left $ "Invalid Name Format ('" ++ name ++ "' contains invalid character(s)"
  | argc < 0 = Left "Negative argument count specified"
  | argc == 0 = Right [Marker name]
  | argc == 1 = Right
                [Marker name
                ,Address (Symbol "SP")
                ,Calculate LoadM (Just StoreA) Nothing -- load the Stack address into A
                ,Calculate Load0 (Just StoreM) Nothing -- store 0 onto the top of the stack
                ,Address (Symbol "SP")
                ,Calculate IncM (Just StoreM) Nothing -- increment the Stack address and store
                ]
  | otherwise = Right
                [Marker name
                ,Address (Literal argc)
                ,Calculate LoadA (Just StoreD) Nothing -- store the number of arguments into D
                ,Marker (name ++ "$INIT") -- create a label for initialization looping
                ,Address (Symbol "SP")
                ,Calculate LoadM (Just StoreA) Nothing -- load the Stack address into A
                ,Calculate Load0 (Just StoreM) Nothing -- store 0 onto the top of the stack
                ,Address (Symbol "SP")
                ,Calculate IncM (Just StoreM) Nothing -- increment the Stack address and store
                ,Calculate DecD (Just StoreD) Nothing -- decrement the completed argument count
                ,Address (Symbol (name ++ "$INIT"))
                ,Calculate LoadD Nothing (Just JumpNE) -- continue initializing unless D is 0
                ]

command2asm file lnum func (Call name paramc)
  | head name `elem` "0123456789" = Left $ "Invalid Name Format ('" ++ name ++ "' cannot start with a digit)"
  | not (all (`elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_.:0123456789") name) = Left $ "Invalid Name Format ('" ++ name ++ "' contains invalid character(s)"
  | paramc < 0 = Left "Negative argument count specified"
  | otherwise = Right $
                [Address (Symbol retname)
                ,Calculate LoadA (Just StoreD) Nothing
                ] ++ pushRoutine ++ -- PUSH RETURN-ADDRESS
                [Address (Symbol "LCL")
                ,Calculate LoadM (Just StoreD) Nothing
                ] ++ pushRoutine ++ -- PUSH LCL
                [Address (Symbol "ARG")
                ,Calculate LoadM (Just StoreD) Nothing
                ] ++ pushRoutine ++ -- PUSH ARG
                [Address (Symbol "THIS")
                ,Calculate LoadM (Just StoreD) Nothing
                ] ++ pushRoutine ++ -- PUSH THIS
                [Address (Symbol "THAT")
                ,Calculate LoadM (Just StoreD) Nothing
                ] ++
                [Address (Symbol "SP") -- MODIFIED PUSH ROUTINE
                ,Calculate LoadM (Just StoreA) Nothing
                ,Calculate LoadD (Just StoreM) Nothing
                ,Address (Symbol "SP")
                ,Calculate IncM (Just StoreMD) Nothing
                ] ++ -- PUSH THAT
                [Calculate LoadM (Just StoreD) Nothing
                ,Address (Literal (paramc + 5))
                ,Calculate SubDA (Just StoreD) Nothing
                ,Address (Symbol "ARG")
                ,Calculate LoadD (Just StoreM) Nothing -- ARG = SP-n-5
                ,Address (Symbol "SP")
                ,Calculate LoadM (Just StoreD) Nothing
                ,Address (Symbol "LCL")
                ,Calculate LoadD (Just StoreM) Nothing -- LCL = SP
                ,Address (Symbol name)
                ,Calculate Load0 Nothing (Just Jump) -- goto f
                ,Marker retname -- (return-address)
                ]
  where
    retname = name ++ "$RET" ++ file ++ func ++ show lnum

command2asm _ _ _ Return = Right $
                           [Address (Symbol "LCL")
                           ,Calculate LoadM (Just StoreD) Nothing
                           ,Address (Symbol "R13")
                           ,Calculate LoadD (Just StoreM) Nothing -- FRAME = LCL
                           ,Address (Literal 5)
                           ,Calculate SubDA (Just StoreA) Nothing
                           ,Calculate LoadM (Just StoreD) Nothing
                           ,Address (Symbol "R14")
                           ,Calculate LoadD (Just StoreM) Nothing -- RET = *(FRAME-5)
                           ,Address (Symbol "SP")
                           ,Calculate DecM (Just StoreA) Nothing
                           ,Calculate LoadM (Just StoreD) Nothing
                           ,Address (Symbol "ARG")
                           ,Calculate LoadM (Just StoreA) Nothing
                           ,Calculate LoadD (Just StoreM) Nothing -- (val)*ARG = pop()
                           ,Calculate IncA (Just StoreD) Nothing
                           ,Address (Symbol "SP")
                           ,Calculate LoadD (Just StoreM) Nothing -- SP = ARG+1
                           ] ++
                           loadSegRoutine "THAT" ++ -- THAT = *(FRAME-1)
                           loadSegRoutine "THIS" ++ -- THIS = *(FRAME-2)
                           loadSegRoutine "ARG" ++ -- ARG = *(FRAME-3)
                           loadSegRoutine "LCL" ++ -- LCL = *(FRAME-4)
                           [Address (Symbol "R14")
                           ,Calculate LoadM (Just StoreA) Nothing
                           ,Calculate Load0 Nothing (Just Jump)
                           ]
  where
    loadSegRoutine sym = [Address (Symbol "R13")
                         ,Calculate DecM (Just StoreAM) Nothing
                         ,Calculate LoadM (Just StoreD) Nothing
                         ,Address (Symbol sym)
                         ,Calculate LoadD (Just StoreM) Nothing
                         ]


--


asmHelperEQ :: [Instruction]
asmHelperEQ = [Marker "$STACKEQ"
              ] ++ popRoutine ++
              [Calculate DecA (Just StoreA) Nothing -- load the previous Stack address into A
              ,Calculate SubDM (Just StoreD) Nothing -- 0 if eq else nonzero
              ,Calculate InvD (Just StoreM) Nothing -- replace the top of the Stack with NOT(D) (0 if neq)
              ,Address (Symbol "$STACKCMPFALSE")
              ,Calculate LoadD Nothing (Just JumpNE) -- jump if the two values were not equal
              ,Address (Symbol "$STACKCMPTRUE")
              ,Calculate Load0 Nothing (Just Jump) -- jump (the two values were equal)
              ]
asmHelperGT :: [Instruction]
asmHelperGT = [Marker "$STACKGT"
              ] ++ popRoutine ++
              [Calculate DecA (Just StoreA) Nothing -- load the previous Stack address into A
              ,Calculate SubDM (Just StoreD) Nothing -- positive if gt
              ,Address (Symbol "$STACKCMPTRUE")
              ,Calculate LoadD Nothing (Just JumpLT)
              ,Address (Symbol "$STACKCMPFALSE")
              ,Calculate Load0 Nothing (Just Jump)
              ]
asmHelperLT :: [Instruction]
asmHelperLT = [Marker "$STACKLT"]
              ++ popRoutine ++
              [Calculate DecA (Just StoreA) Nothing -- load the previous Stack address into A
              ,Calculate SubDM (Just StoreD) Nothing -- positive if lt
              ,Address (Symbol "$STACKCMPTRUE")
              ,Calculate LoadD Nothing (Just JumpGT)
              ,Address (Symbol "$STACKCMPFALSE")
              ,Calculate Load0 Nothing (Just Jump)
              ]
asmHelperCMP :: [Instruction]
asmHelperCMP = asmHelperGT ++ asmHelperLT ++ asmHelperEQ ++
               [Marker "$STACKCMPTRUE" -- TRUE comparison result
               ,Address (Symbol "SP")
               ,Calculate DecM (Just StoreA) Nothing
               ,Calculate LoadN1 (Just StoreM) Nothing -- store -1 (all 1s) at the top of the stack
               ,Address (Symbol "$STACKCMP_")
               ,Calculate Load0 Nothing (Just Jump) -- jump past the FALSE case
               ,Marker "$STACKCMPFALSE" -- FALSE comparison result
               ,Address (Symbol "SP")
               ,Calculate DecM (Just StoreA) Nothing
               ,Calculate Load0 (Just StoreM) Nothing
               ,Marker "$STACKCMP_" -- finish comparing
               ,Address (Symbol "R15")
               ,Calculate LoadM (Just StoreA) Nothing
               ,Calculate Load0 Nothing (Just Jump)
               ]

boilerplate :: [Instruction]
-- necessary code to be placed at the end of the ASM output file
-- contains and endless runloop as well as the CMP subroutines
boilerplate = [Address (Symbol "$$$FINISHED")
              ,Marker "$$$FINISHED"
              ,Calculate Load0 Nothing (Just Jump)
              ] ++ asmHelperCMP

bootloader :: [Instruction]
-- necessary code to be placed at the beginning of the ASM output file
-- contains the SP initializer, Sys.init call, and boilerplate code
bootloader = [Address (Literal 256)
             ,Calculate LoadA (Just StoreD) Nothing
             ,Address (Symbol "SP")
             ,Calculate LoadD (Just StoreM) Nothing -- set the Stack address to 256
             ] ++ initializer ++ boilerplate
  where
    initcode = command2asm "" 0 "" (Call "Sys.init" 0)
    initializer = either (error "THIS IS A MAJOR FAILURE ON THE DEVELOPER'S PART") id initcode


---


translateLine :: String -> Integer -> String -> Command -> Either String [Instruction]
translateLine = command2asm


translateFunction :: String -> [Command] -> Either [String] [Instruction]
translateFunction file ((Function function args):remainder) = if null errors
                                                              then Right (concat instructions)
                                                              else Left errors
  where
    commands = ((Function function args):remainder)
    (errors,instructions) = partitionEithers $ map (\(a,b) -> translateLine file a function b) $ zip [1..] commands
translateFunction _ _ = error "THIS IS A MAJOR FAILURE ON THE DEVELOPER'S PART"

isFunction :: Command -> Bool
isFunction (Function _ _) = True
isFunction _ = False

translateFile :: String -> [Command] -> Either [String] [Instruction]
translateFile file ((Function function args):remainder) = if null errors
                                                          then Right (concat instructions)
                                                          else Left (concat errors)
  where
    commands = ((Function function args):remainder)
    functiongroups = groupBy (\_ a -> not (isFunction a)) commands
    (errors,instructions) = partitionEithers $ map (translateFunction file) functiongroups
translateFile _ _ = Left ["File does not start with a function declaration (code cannot appear outside of a function)"]

translate :: [(String,[Command])] -> Either [String] [Instruction]
-- ^Translates VM code into Assembly; provides informative error strings on failure
translate entries = if null (concat errors)
                    then Right (bootloader ++ concat instructions)
                    else Left (concat errors)
  where
    (errors,instructions) = partitionEithers $ map (uncurry translateFile) entries

dropComment :: String -> String
dropComment str = take (length $ takeWhile ((/="//") . take 2) $ tails str) str

parseVM :: String -> Either [String] [Command]
-- ^Transforms VM source code into its virtual representation; provides informative error strings on failure
parseVM input = if not (null badcommands)
                then Left (map (\(a,b) -> "Line " ++ show a ++ " (" ++ b ++ "): could not parse") inputlines)
                else Right (catMaybes potentialcommands)
  where
    inputlines = filter (not . all isSpace . snd) $ zip [1..] $ map dropComment $ lines input
    potentialcommands = map (readMaybe . snd) inputlines
    badcommands = map (\a -> inputlines !! a) $ elemIndices Nothing potentialcommands
