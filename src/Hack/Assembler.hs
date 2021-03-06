{-|
The Assembler defines the valid Hack assembly instructions and provides tools for manipulating assembly, including the actual act of assembling.
-}
module Hack.Assembler
( Instruction(..)
, Constant(..)
, Calculation(..)
, Destination(..)
, Jump(..)
, assemble
, parseAssembly
) where

import Data.Char (isSpace, isDigit)
import Data.Either (lefts, rights, partitionEithers)
import Data.List (partition, tails)
import qualified Data.Map as Map (Map, (!), fromList, intersection, member, null, union, toList)
import Data.Maybe (fromJust, isNothing, mapMaybe)
import qualified Data.Set as Set (Set, empty, insert, member, toList)
import Data.Word (Word16)
import Text.Read (readMaybe)



duplicates :: Ord a => [a] -> Set.Set a
-- ^Identifies the duplicates in a list
duplicates ys = dups ys Set.empty Set.empty
  where
    dups [] _ ds = ds
    dups (x:xs) seen ds = dups xs (Set.insert x seen) (if Set.member x seen then Set.insert x ds else ds)


-- |The constants that can be addressed by an address command
data Constant
  = Literal Int -- ^A literal value
  | Symbol String -- ^A symbol that will be translated into a value
instance Show Constant where
  show (Literal i) = show i
  show (Symbol s) = tail (init (show s))

-- |Defines the possible calculations that can be made by the ALU
data Calculation = Load0 | Load1 | LoadN1 |
                   LoadD | LoadA | InvD |
                   InvA | NegD | NegA |
                   IncD | IncA | DecD | DecA |
                   AddDA | SubDA | SubAD | AndDA | OrDA |
                   LoadM | InvM | NegM | IncM | DecM |
                   AddDM | SubDM | SubMD | AndDM | OrDM
instance Show Calculation where
  show Load0 = "0"
  show Load1 = "1"
  show LoadN1 = "-1"
  show LoadD = "D"
  show LoadA = "A"
  show InvD = "!D"
  show InvA = "!A"
  show NegD = "-D"
  show NegA = "-A"
  show IncD = "D+1"
  show IncA = "A+1"
  show DecD = "D-1"
  show DecA = "A-1"
  show AddDA = "D+A"
  show SubDA = "D-A"
  show SubAD = "A-D"
  show AndDA = "D&A"
  show OrDA = "D|A"
  show LoadM = "M"
  show InvM = "!M"
  show NegM = "-M"
  show IncM = "M+1"
  show DecM = "M-1"
  show AddDM = "D+M"
  show SubDM = "D-M"
  show SubMD = "M-D"
  show AndDM = "D&M"
  show OrDM = "D|M"
instance Read Calculation where
  readsPrec _ ('D':'+':'1':xs) = [(IncD,xs)]
  readsPrec _ ('A':'+':'1':xs) = [(IncA,xs)]
  readsPrec _ ('D':'-':'1':xs) = [(DecD,xs)]
  readsPrec _ ('A':'-':'1':xs) = [(DecA,xs)]
  readsPrec _ ('D':'+':'A':xs) = [(AddDA,xs)]
  readsPrec _ ('D':'-':'A':xs) = [(SubDA,xs)]
  readsPrec _ ('A':'-':'D':xs) = [(SubAD,xs)]
  readsPrec _ ('D':'&':'A':xs) = [(AndDA,xs)]
  readsPrec _ ('D':'|':'A':xs) = [(OrDA,xs)]
  readsPrec _ ('M':'+':'1':xs) = [(IncM,xs)]
  readsPrec _ ('M':'-':'1':xs) = [(DecM,xs)]
  readsPrec _ ('D':'+':'M':xs) = [(AddDM,xs)]
  readsPrec _ ('D':'-':'M':xs) = [(SubDM,xs)]
  readsPrec _ ('M':'-':'D':xs) = [(SubMD,xs)]
  readsPrec _ ('D':'&':'M':xs) = [(AndDM,xs)]
  readsPrec _ ('D':'|':'M':xs) = [(OrDM,xs)]
  readsPrec _ ('-':'1':xs)     = [(LoadN1,xs)]
  readsPrec _ ('!':'D':xs)     = [(InvD,xs)]
  readsPrec _ ('!':'A':xs)     = [(InvA,xs)]
  readsPrec _ ('-':'D':xs)     = [(NegD,xs)]
  readsPrec _ ('-':'A':xs)     = [(NegA,xs)]
  readsPrec _ ('!':'M':xs)     = [(InvM,xs)]
  readsPrec _ ('-':'M':xs)     = [(NegM,xs)]
  readsPrec _ ('0':xs)         = [(Load0,xs)]
  readsPrec _ ('1':xs)         = [(Load1,xs)]
  readsPrec _ ('D':xs)         = [(LoadD,xs)]
  readsPrec _ ('A':xs)         = [(LoadA,xs)]
  readsPrec _ ('M':xs)         = [(LoadM,xs)]
  readsPrec _ _ = []

-- |Defines the possible locations in which to store the result of a calculation
data Destination
  = StoreM -- ^Stores the result into Memory[A-register]
  | StoreD -- ^Stores the result into the D-register
  | StoreMD
  | StoreA -- ^Stores the result into the A-register
  | StoreAM
  | StoreAD
  | StoreAMD
instance Show Destination where
  show StoreM = "M"
  show StoreD = "D"
  show StoreMD = "MD"
  show StoreA = "A"
  show StoreAM = "AM"
  show StoreAD = "AD"
  show StoreAMD = "AMD"
instance Read Destination where
  readsPrec _ ('A':'M':'D':'=':xs) = [(StoreAMD,xs)]
  readsPrec _ ('M':'D':'=':xs) = [(StoreMD,xs)]
  readsPrec _ ('A':'M':'=':xs) = [(StoreAM,xs)]
  readsPrec _ ('A':'D':'=':xs) = [(StoreAD,xs)]
  readsPrec _ ('M':'=':xs) = [(StoreM,xs)]
  readsPrec _ ('D':'=':xs) = [(StoreD,xs)]
  readsPrec _ ('A':'=':xs) = [(StoreA,xs)]
  readsPrec _ _ = []

-- |Defines the possible jumps that can occur after performing a calculation
data Jump
  = JumpGT -- ^Jumps iff calc > 0
  | JumpEQ -- ^Jumps iff calc == 0
  | JumpGE -- ^Jumps iff calc >= 0
  | JumpLT -- ^Jumps iff calc < 0
  | JumpNE -- ^Jumps iff calc /= 0
  | JumpLE -- ^Jumps iff calc <= 0
  | Jump   -- ^Jumps unconditionally
instance Show Jump where
  show JumpGT = "JGT"
  show JumpEQ = "JEQ"
  show JumpGE = "JGE"
  show JumpLT = "JLT"
  show JumpNE = "JNE"
  show JumpLE = "JLE"
  show Jump = "JMP"
instance Read Jump where
  readsPrec _ (';':'J':'G':'T':xs) = [(JumpGT,xs)]
  readsPrec _ (';':'J':'E':'Q':xs) = [(JumpEQ,xs)]
  readsPrec _ (';':'J':'G':'E':xs) = [(JumpGE,xs)]
  readsPrec _ (';':'J':'L':'T':xs) = [(JumpLT,xs)]
  readsPrec _ (';':'J':'N':'E':xs) = [(JumpNE,xs)]
  readsPrec _ (';':'J':'L':'E':xs) = [(JumpLE,xs)]
  readsPrec _ (';':'J':'M':'P':xs) = [(Jump,xs)]
  readsPrec _ _ = []

-- |A single instruction that may be executed in a single cycle of the machine
data Instruction
  = Address Constant -- ^ A constant that will be loaded into memory
  | Marker String -- ^ A label that identifies a location in the program
  | Calculate Calculation (Maybe Destination) (Maybe Jump) -- ^ A calculation for the ALU to perform
instance Show Instruction where
  show (Address c) = '@':show c
  show (Marker s) = "(" ++ (tail (init (show s))) ++ ")"
  show (Calculate c Nothing Nothing) = show c
  show (Calculate c Nothing (Just j)) = show c ++ ";" ++ show j
  show (Calculate c (Just d) Nothing) = show d ++ "=" ++ show c
  show (Calculate c (Just d) (Just j)) = show d ++ "=" ++ show c ++ ";" ++ show j


-- converts a calculation to binary
c2wrd :: Calculation -> Word16
c2wrd Load0 = 2688
c2wrd Load1 = 4032
c2wrd LoadN1 = 3712
c2wrd LoadD = 768
c2wrd LoadA = 3072
c2wrd InvD = 832
c2wrd InvA = 3136
c2wrd NegD = 960
c2wrd NegA = 3264
c2wrd IncD = 1984
c2wrd IncA = 3520
c2wrd DecD = 896
c2wrd DecA = 3200
c2wrd AddDA = 128
c2wrd SubDA = 1216
c2wrd SubAD = 448
c2wrd AndDA = 0
c2wrd OrDA = 1344
c2wrd LoadM = 7168
c2wrd InvM = 7232
c2wrd NegM = 7360
c2wrd IncM = 7616
c2wrd DecM = 7296
c2wrd AddDM = 4224
c2wrd SubDM = 5312
c2wrd SubMD = 4544
c2wrd AndDM = 4096
c2wrd OrDM = 5440
-- converts a destination to binary
d2wrd :: Maybe Destination -> Word16
d2wrd Nothing = 0
d2wrd (Just StoreM) = 8
d2wrd (Just StoreD) = 16
d2wrd (Just StoreMD) = 24
d2wrd (Just StoreA) = 32
d2wrd (Just StoreAM) = 40
d2wrd (Just StoreAD) = 48
d2wrd (Just StoreAMD) = 56
-- converts a jump to binary
j2wrd :: Maybe Jump -> Word16
j2wrd Nothing = 0
j2wrd (Just JumpGT) = 1
j2wrd (Just JumpEQ) = 2
j2wrd (Just JumpGE) = 3
j2wrd (Just JumpLT) = 4
j2wrd (Just JumpNE) = 5
j2wrd (Just JumpLE) = 6
j2wrd (Just Jump) = 7

-- Assembles an instruction to bits
-- ERROR if the instruction is symbolic (Marker _ or Address Symbol _)
-- EXCEPTION if an address literal is greater than memory capacity
assembleIns :: Instruction -> Either String Word16
assembleIns (Marker _) = error "Attempted to assemble a label"
assembleIns (Address (Symbol _)) = error "Attempted to assemble a symbolic address"
assembleIns (Address (Literal lit)) = if (lit > 32767) || (lit < 0)
                                      then Left ("Address literal '" ++ show lit ++ "' out of range (0 <= addr <= 32767)")
                                      else Right (fromIntegral lit)
assembleIns (Calculate c d j) = Right (57344 + c2wrd c + d2wrd d + j2wrd j)


-- Determines if a symbol is of the proper syntax
isValidSymbol :: String -> Bool
isValidSymbol sym
    | head sym `elem` "0123456789" = False
    | otherwise = all (`elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_.$:0123456789") sym


-- The predefined symbols for the assembly language
syssymbols :: Map.Map String Word16
syssymbols = Map.fromList [("SP",0),("LCL",1),("ARG",2),
                           ("THIS",3),("THAT",4),
                           ("R0",0),("R1",1),("R2",2),("R3",3),
                           ("R4",4),("R5",5),("R6",6),("R7",7),
                           ("R8",8),("R9",9),("R10",10),("R11",11),
                           ("R12",12),("R13",13),("R14",14),("R15",15),
                           ("SCREEN",16384),("KBD",24576)]


-- Determines if an instruction is a label instruction
isMarker :: Instruction -> Bool
isMarker (Marker _) = True
isMarker _ = False


-- Removes the label instructions and provides a map of labels to binary
-- EXCEPTION if the syntax for a label is bad or if a label was declared multiple times
unlabel :: [Instruction] -> Either String ([Instruction], Map.Map String Word16)
unlabel instructions
  | not (null badMarkers) = Left $ "Marker(s) " ++ show badMarkers ++ " are of invalid syntax (any sequence of letters, digits, _, ., $, : that does not begin with a digit)"
  | not (null dupes) = Left $ "Marker(s) " ++ show dupes ++ " declared multiple types"
  | not (Map.null inters) = Left $ "Symbol(s) " ++ show (map fst $ Map.toList inters) ++ " are reserved but were declared as labels"
  | otherwise = Right (map fst finalins, syssymbols `Map.union` labelmap)
  where
    lineseq = scanl (\a b -> if isMarker b then a else a + 1) 0 instructions
    (labels,finalins) = partition (isMarker . fst) $ zip instructions lineseq
    slabeltrans (Marker a, b) = (a,b)
    slabeltrans _ = error "Attempted to translate a non-Marker command"
    slabels = map slabeltrans labels
    badMarkers = filter (not . isValidSymbol) $ map fst slabels
    dupes = Set.toList $ duplicates $ map fst slabels
    labelmap = Map.fromList slabels
    inters = Map.intersection syssymbols labelmap

-- Removes the variables from instructions using a label map
-- ERROR if a label instruction is part of the instructions (should have been filtered out already)
-- EXCEPTION if the syntax for a variable is invalid or if there are too many variables to assign memory addresses
unvariable :: [Instruction] -> Map.Map String Word16 -> Either String [Instruction]
unvariable instructions lmap
  | not (null labels) = error "Found a Marker while parsing out symbols"
  | not (null badSyms) = Left $ "Variable(s) " ++ show badSyms ++ " are of invalid syntax (any sequence of letters, digits, _, ., $, : that does not begin with a digit)"
  | length ssyms > 16368 = Left $ "Too many variables to fit into memory (exceeds memory by " ++ show (length ssyms - 16368) ++ ")"
  | otherwise = Right $ map (\a -> unvar a (lmap `Map.union` msyms)) instructions
  where
    labels = filter isMarker instructions
    getSymbol (Address (Symbol s)) = Just s
    getSymbol _ = Nothing
    ssyms = filter (\a -> not (Map.member a lmap)) $ foldl (\seen x -> if x `elem` seen then seen else seen ++ [x]) [] $ mapMaybe getSymbol instructions -- removes duplicates
    badSyms = filter (not . isValidSymbol) ssyms
    msyms = Map.fromList $ zip ssyms [(16 :: Word16)..(16368 :: Word16)]
    unvar (Address (Symbol s)) smap = Address (Literal (fromIntegral (smap Map.! s)))
    unvar i _ = i

-- Removes all symbols from instructions
-- ERROR if (see unlabel unvariable)
-- EXCEPTION if (see unlabel unvariable)
unsymbol :: [Instruction] -> Either String [Instruction]
unsymbol instructions = either Left (\(newins,smap) -> either Left Right (unvariable newins smap)) $ unlabel instructions


-- Assembles instructions to binary
-- ERROR if (see unsymbol assembleIns)
-- EXCEPTION if (see unsymbol assembleIns)
assemble :: [Instruction] -> Either [String] [Word16]
-- ^Assembles instructions to their 16-bit binary form; provides informative error strings on failure
assemble instructions = either (\a -> Left [a]) (\b -> if isOkay b then Right (rs b) else Left (ls b)) $ unsymbol instructions
  where
    asmd = map assembleIns
    rs = rights . asmd
    ls = lefts . asmd
    isOkay = null . ls






pOAC :: String -> Either String Instruction
pOAC input
  | null compstr = Left "No calculation specified"
  | isNothing comp = Left "Invalid calculation specified"
  | isNothing dest && isNothing jump = Right (Calculate (fromJust comp) Nothing Nothing)
  | isNothing dest = if isNothing (fromJust jump)
                     then Left "Invalid jump specified"
                     else Right (Calculate (fromJust comp) Nothing (fromJust jump))
  | isNothing jump = if isNothing (fromJust dest)
                     then Left "Invalid destination specified"
                     else Right (Calculate (fromJust comp) (fromJust dest) Nothing)
  | otherwise = if isNothing (fromJust dest)
                then Left "Invalid destination specified"
                else if isNothing (fromJust jump)
                     then Left "Invalid jump specified"
                     else Right (Calculate (fromJust comp) (fromJust dest) (fromJust jump))
  where
    dest = if '=' `elem` input
           then Just (readMaybe (takeWhile (/='=') input ++ "="))
           else Nothing
    jump = if ';' `elem` input
           then Just (readMaybe (dropWhile (/=';') input))
           else Nothing
    compstr = takeWhile (/=';') (if '=' `elem` input
                                 then tail (dropWhile (/='=') input)
                                 else input)
    comp = readMaybe compstr
    

parseOneAssembly :: String -> Either String Instruction
parseOneAssembly string
  | start == '(' = if end /= ')'
                   then Left "Marker does not end with a close paren ')'"
                   else Right (Marker center)
  | start == '@' = Right (Address (if all isDigit center then Literal (read (tail string)) else Symbol (tail string)))
--  | start == '@' = if all isDigit center
--                   then Right (Address (Literal (read (tail string))))
--                   else Right (Address (Symbol (tail string)))
  | otherwise = pOAC string
  where
    start = head string
    end = last string
    center = init (tail string)


lol :: (Integer,String) -> Either String Instruction
lol (lnum,input) = either (\a -> Left ("Line " ++ show lnum ++ ": " ++ a)) Right $ parseOneAssembly input

dropComment :: String -> String
dropComment str = take (length $ takeWhile ((/="//") . take 2) $ tails str) str

parseAssembly :: String -> Either [String] [Instruction]
-- ^Transforms source assembly code into assembly instructions; provides informative error strings on failure
parseAssembly input = if null errors
                      then Right ins
                      else Left errors
  where
    strins = filter (not . null . snd) $ zip [1..] $ map (dropComment . filter (not . isSpace)) (lines input)
    (errors,ins) = partitionEithers $ map lol strins
