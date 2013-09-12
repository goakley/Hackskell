module Main where

import Hack.Assembler

import Control.Arrow (second)
import Data.Word (Word16)
import System.Environment (getArgs)
import System.FilePath (addExtension, dropExtension)


word2bin :: Word16 -> String
word2bin val = if val > 32767 then pady else '0':pady
    where
      word2binM 0 = "0"
      word2binM 1 = "1"
      word2binM v = let (d,b) = divMod v 2 in word2binM d ++ word2binM b
      ib = word2binM val
      pady = replicate (15 - length ib) '0' ++ ib


isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False


main :: IO ()
main = do
  filenamess <- getArgs
  let filenames = if not (null filenamess) then filenamess else ["_stdin_.asm"]
  contentss <- if not (null filenamess) then mapM readFile filenames else sequence [getContents]
  let parsed = map (second parseAssembly) $ zip filenames contentss
  let parserrors = filter (\(_,b) -> not (null b)) $ map (\(name,txt) -> either (\a -> (name,a)) (const (name,[])) txt) parsed
  let asms = filter (\(_,b) -> not (null b)) $ map (\(name,txt) -> either (const (name,[])) (\b -> (name,b)) txt) parsed
  let assembled = map (second assemble) asms
  let asmerrors = filter (\(_,b) -> not (null b)) $ map (\(name,txt) -> either (\a -> (name,a)) (const (name,[])) txt) assembled
  let asmds = filter (\(_,b) -> not (null b)) $ map (\(name,txt) -> either (const (name,[])) (\b -> (name,b)) txt) assembled
  mapM_ (\(a,b) -> (mapM_ (putStrLn . (\c -> a ++ ":" ++ c)) b)) $ parserrors ++ asmerrors
  mapM_ (\(a,b) -> writeFile ((flip addExtension ".hack" . dropExtension) a) (unlines (map word2bin b))) asmds
