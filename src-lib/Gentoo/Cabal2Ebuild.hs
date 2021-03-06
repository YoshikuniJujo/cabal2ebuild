module Gentoo.Cabal2Ebuild (cabal2ebuild) where

import Distribution.Package
-- import Distribution.Version
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Data.Maybe
import Data.Version
import Distribution.License
-- import System.Directory
import Data.List

import Gentoo.Depend

import System.IO.Unsafe
import System.Directory
import System.FilePath
import Control.Applicative

test :: IO ()
test = do
  gpd <- readPackageDescription normal "cabal2ebuild.cabal"
  print $ getName gpd ++ ".ebuild"
  print $ getName gpd ++ ".tar.gz"
  print $ getLicense gpd
  print $ hasLib gpd
  print $ hasEx gpd
  putStrLn $ makeCabalFeatures ( hasLib gpd ) ( hasEx gpd )
  putStrLn $ makeSrcURI $ getName gpd
  putStrLn ""
--  putStrLn $ makeEbuild gpd
  print $ condTreeData $ snd $ head $ condExecutables gpd
  mapM_ print $ condTreeConstraints $ snd $ head $ condExecutables gpd
  print $ synopsis $ packageDescription gpd
  putStrLn ""
  putStr $ ebuildToFile $ gpdToEbuild gpd
  print $ condLibrary gpd
  print $ ebDepends gpd
  

cabal2ebuild :: String -> Maybe (FilePath, String)
cabal2ebuild cnt = case parsePackageDescription cnt of
	ParseOk _ gpd -> Just (makeFileName gpd, ebuildToFile $ gpdToEbuild gpd)
	_ -> Nothing

makeFileName :: GenericPackageDescription -> String
makeFileName gpd = getName gpd ++ ".ebuild"

-- data EbLicense = BSD3 | GPL | LGPL deriving Show

data EbKWords = X86 | BX86 | AMD64 deriving Show

showEKW :: EbKWords -> String
showEKW X86  = "x86"
showEKW BX86 = "~x86"
showEKW AMD64 = "amd64"

data Ebuild = Ebuild {
 
  ebCabalFt :: [ String ] ,
  ebInherit :: String ,
  ebAPI     :: Int ,
  ebDsc     :: String ,
  ebHmpg    :: String ,
  srcURI    :: String ,
  ebLicense :: License ,
  ebSlot    :: Int ,
  ebKWords  :: [ EbKWords ] ,
  ebDepend  :: [ String ]

 } deriving Show

gpdToEbuild :: GenericPackageDescription -> Ebuild
gpdToEbuild gpd = Ebuild {

  ebCabalFt = ft ,
  ebInherit = "haskell-cabal",
  ebAPI     = 3 ,
  ebDsc     = synopsis $ packageDescription gpd ,
  ebHmpg    = homepage $ packageDescription gpd ,
  srcURI    = makeSrcURI ( getName gpd ) ,
  ebLicense = getLicense gpd ,
  ebSlot    = 0 ,
  ebKWords  = [ X86, AMD64 ] ,
  ebDepend  = ">=dev-lang/ghc-6.10" : "dev-haskell/cabal" : ebDepends gpd

 }
  where
  ft :: [ String ]
  ft = concatMap snd $ filter fst
    [ ( hasLib gpd , [ "lib", "haddock", "profile", "hscolour" ] ),
      ( hasEx gpd  , [ "bin" ] ) ] 
  
ebuildToFile :: Ebuild -> String
ebuildToFile eb =
  "EAPI=" ++ show ( ebAPI eb ) ++ "\n\n" ++
  "CABAL_FEATURES=\"" ++ unwords ( ebCabalFt eb ) ++ "\"\n" ++
  "inherit " ++ ebInherit eb ++ "\n\n" ++
  "DESCRIPTION=" ++ show ( ebDsc eb ) ++ "\n" ++
  "HOMEPAGE=" ++ show ( ebHmpg eb ) ++ "\n" ++
  "SRC_URI=" ++ srcURI eb ++ "\n\n" ++
  "LICENSE=\"" ++ show ( ebLicense eb ) ++ "\"\n" ++
  "SLOT=\"" ++ show ( ebSlot eb ) ++ "\"\n" ++
  "KEYWORDS=\"" ++ unwords ( map showEKW $ ebKWords eb ) ++ "\"\n\n" ++
  "DEPEND=\"" ++ unlines ( ebDepend eb ) ++ "\"\n"

makeEbuild :: GenericPackageDescription -> String
makeEbuild gpd =
  makeCabalFeatures ( hasLib gpd ) ( hasEx gpd ) ++ "\n" ++
  "inherit haskell-cabal\n\n" ++
  "EAPI=3\n\n" ++
  "SRC_URI=" ++ makeSrcURI ( getName gpd ) ++ "\n\n" ++
  makeLicense ( getLicense gpd ) ++ "\n" ++
  "SLOT=\"0\"\n" ++ "KEYWORDS=\"x86\"\n"

makeCabalFeatures :: Bool -> Bool -> String
makeCabalFeatures lb ex
  = let ls = if lb then "lib " else ""
        xs = if ex then "bin " else ""
     in "CABAL_FEATURES=" ++ show ( xs ++ ls ++ "haddock profile hscolour" )

myGentooRepo :: Maybe String
myGentooRepo = unsafePerformIO $ do
	hd <- getHomeDirectory
	let confFile = hd </> ".cabal2ebuild" </> "gentoo_repo.txt"
	fe <- doesFileExist confFile
	if not fe then return Nothing else
		fromConfigFile <$> readFile confFile

fromConfigFile :: String -> Maybe String
fromConfigFile cnt = case lines cnt of
	["local", repo] -> Just repo
	_ -> Nothing

makeSrcURI :: String -> String
-- makeSrcURI nam = show ( "http://homepage3.nifty.com/salamander/second/portage/distfiles/" ++ nam ++ ".tar.gz" )
makeSrcURI nam = case myGentooRepo of
	Just r -> show $ r ++ nam ++ ".tar.gz"
	_ -> show $ "http://hackage.haskell.org/package/" ++ nam ++ "/" ++
		nam ++ ".tar.gz"

makeLicense :: License -> String
makeLicense l = "LICENSE=" ++ show ( show l )

getName :: GenericPackageDescription -> String
getName gpd = let pd = packageDescription gpd
                  PackageName pn = pkgName $ package pd
                  vn = showVersion $ pkgVersion $ package pd
               in pn ++ "-" ++ vn

getLicense :: GenericPackageDescription -> License
getLicense = license . packageDescription

hasLib :: GenericPackageDescription -> Bool
hasLib = isJust . condLibrary

hasEx :: GenericPackageDescription -> Bool
hasEx = not . null . condExecutables
