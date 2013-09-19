-- Tests the assembler by reading in the files from the ASM program directory
-- and testing the assembled result against the matching files in the HACK program directory

import Hack.Assembler

import Control.Arrow ((***))
import Control.Monad (when)
import Data.Char (digitToInt)
import Data.Word (Word16)
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))


assert :: Bool -> a -> a
assert False x = error "Assert failed!"
assert True  x = x

bin2wrd :: String -> Word16
bin2wrd s = foldl (\a b -> 2 * a + fromIntegral (digitToInt b)) 0 $ dropWhile (=='0') s

readHack :: String -> [Word16]
readHack s = map bin2wrd $ lines s

readAsm :: FilePath -> String -> [Word16]
readAsm n s = either (\a -> error (show (("Failed during parsing step in " ++ n):a))) (either (\b -> error (show (("Failed during assembling step in " ++ n):b))) id . assemble) $ parseAssembly s

readPair :: (FilePath,FilePath) -> IO ((FilePath,String),(FilePath,String))
readPair (a,b) = do
  am <- readFile a
  bm <- readFile b
  return ((a,am),(b,bm))

main = do
  cwd <- getCurrentDirectory
  let dirasm = cwd </> "programs" </> "asm"
  let dirhack = cwd </> "programs" </> "hack"
  let filepairs = [("add.asm","add.hack")
                  ,("max.asm","max.hack"),("MaxL.asm","max.hack")
                  ,("pong.asm","pong.hack"),("PongL.asm","pong.hack")
                  ,("rect.asm","rect.hack"),("RectL.asm","rect.hack")]
  let filepaths = map ((</>) dirasm *** (</>) dirhack) filepairs
  contentpairs <- mapM readPair filepaths
  let wordpairs = map (\((an,a),(bn,b)) -> ((an,readAsm an a), (bn,readHack b))) contentpairs
  mapM_ (\((an,a),(_,b)) -> when (a /= b) $ error ("Incorrect assembly result for " ++ an)) wordpairs
