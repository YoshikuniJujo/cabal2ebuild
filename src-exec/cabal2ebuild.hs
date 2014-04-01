import Gentoo.Cabal2Ebuild (cabal2ebuild)

import Data.List
import Control.Applicative
import System.Directory

main :: IO ()
main = do
  cabal <- head . filter (isSuffixOf ".cabal") <$> getDirectoryContents "."
  maybe (putStrLn "parse error") (uncurry writeFile)
  	=<< cabal2ebuild <$> readFile cabal
