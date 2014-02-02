module Main where

import Hack.Translator

import Control.Arrow (second)
import System.Environment (getArgs)


isLeft (Left _) = True
isLeft _ = False
isRight (Right _) = True
isRight _ = False
fromLeft (Left a) = a
fromRight (Right b) = b


main :: IO ()
main = do
  -- Treat all arguments as filenames
  filenamess <- getArgs
  -- If there are no input files, use STDIN as a file
  let filenames = if not (null filenamess)
                  then filenamess
                  else ["_stdin_.vm"]
  -- Read all the files in; if there are no input files, read STDIN
  contentss <- if not (null filenamess)
               then mapM readFile filenames
               else sequence [getContents]
  -- Attempt to parse each input file into VM
  let parsed = map (second parseVM) $ zip (map (filter (`elem` "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_.:0123456789")) filenames) contentss
  let parserrors = filter (\(_,b) -> not (null b)) $
                   map (\(name,txt) -> either (\a -> (name,a)) (const (name,[])) txt) parsed
  let vms = filter (\(_,b) -> not (null b)) $
             map (\(name,txt) -> either (const (name,[])) (\b -> (name,b)) txt) parsed
  -- Attempt to translate all successfully-parsed files
  let result = translate vms
  if isLeft result
    then mapM_ (putStrLn . (\c -> "???:" ++ c)) $ (concat (map snd parserrors)) ++ (fromLeft result)
    else mapM_ (\a -> putStrLn (show a)) (fromRight result)
