import Depend
import Distribution.Package
import Distribution.Version
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Verbosity
import Data.Maybe

main :: IO ()
main = do
  gpd <- readPackageDescription normal "cabal2ebuild.cabal"
--  print $ snd $ head $ condExecutables gpd
--  mapM_ print $ condTreeConstraints $ snd $ head $ condExecutables gpd
--  mapM_ ( print . ebPkgName ) $ condTreeConstraints $ snd $ head $ condExecutables gpd
--  mapM_ ( print . ebPkgVersion ) $ condTreeConstraints $ snd $ head $ condExecutables gpd
--  let exdep = catMaybes $ map ebDepend $ concatMap condTreeConstraints $ map snd $ condExecutables gpd
--        libdep = concat $ maybeToList $ fmap ( catMaybes . map ebDepend . condTreeConstraints ) $ condLibrary gpd
--  print $ map snd $ condExecutables gpd
--  print $ fmap ( map ebDepend . condTreeConstraints ) $ condLibrary gpd
--  print $ exdep ++ libdep
  print $ ebDepends gpd
