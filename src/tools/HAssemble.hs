module Main where

import Hack.Assembler

import Control.Arrow (second)
import Data.Word (Word16)
import System.Environment (getArgs)
import System.FilePath (addExtension, dropExtension)


-- Convert a Word to a padded string of 1s and 0s that represent the number in binary
word2bin :: Word16 -> String
word2bin val = if val > 32767 then pady else '0':pady
    where
      word2binM 0 = "0"
      word2binM 1 = "1"
      word2binM v = let (d,b) = divMod v 2 in word2binM d ++ word2binM b
      ib = word2binM val
      pady = replicate (15 - length ib) '0' ++ ib


main :: IO ()
main = do
  -- Treat all arguments as filenames
  filenamess <- getArgs
  -- If there are no input files, use STDIN as a file
  let filenames = if not (null filenamess)
                  then filenamess
                  else ["_stdin_.asm"]
  -- Read all the files in; if there are no input files, read STDIN
  contentss <- if not (null filenamess)
               then mapM readFile filenames
               else sequence [getContents]
  -- Attempt to parse each input file into Assembly
  let parsed = map (second parseAssembly) $ zip filenames contentss
  let parserrors = filter (\(_,b) -> not (null b)) $
                   map (\(name,txt) -> either (\a -> (name,a)) (const (name,[])) txt) parsed
  let asms = filter (\(_,b) -> not (null b)) $
             map (\(name,txt) -> either (const (name,[])) (\b -> (name,b)) txt) parsed
  -- Attempt to assemble all successfully-parsed files
  let assembled = map (second assemble) asms
  let asmerrors = filter (\(_,b) -> not (null b)) $
                  map (\(name,txt) -> either (\a -> (name,a)) (const (name,[])) txt) assembled
  let asmds = filter (\(_,b) -> not (null b)) $
              map (\(name,txt) -> either (const (name,[])) (\b -> (name,b)) txt) assembled
  -- Display all of the errors encountered when attempting to parse and assemble
  mapM_ (\(a,b) -> (mapM_ (putStrLn . (\c -> a ++ ":" ++ c)) b)) $ parserrors ++ asmerrors
  -- Output the properly assembled files to their locations
  mapM_ (\(a,b) -> (if a == "_stdin_.asm" then putStr else writeFile ((flip addExtension ".hack" . dropExtension) a)) (unlines (map word2bin b))) asmds
